package tungsten

import org.junit.Test
import org.junit.Assert._
import java.io._
import Utilities._
import ModuleIO._

class ModuleIOTest  // generate a classfile so Buildr doesn't rebuild this file unnecessarily

class ModuleIOReadTextTest {
  @Test
  def renameInScopeTest {
    assertEquals(symbolFromString("foo.bar.baz"), renameInScope("%bar.baz", List("foo")))
    assertEquals(symbolFromString("a"), renameInScope("@a", List("foo")))
  }

  @Test
  def processAstTest {
    val struct = Struct("@s", List("@s.f", "@s.g"))
    val f = Field("%f", StructType("@a"))
    val g = Field("%g", StructType("%b"))
    val ast = AstNode(struct, List(AstNode(f, Nil), AstNode(g, Nil)))
    val expected = List(Struct("s", List("s.f", "s.g")),
                        Field("s.f", StructType("a")),
                        Field("s.g", StructType("s.b")))
    assertEquals(expected, processAst(ast, Nil))
  }

  @Test
  def parseTest {
    val program = "global unit @g"
    val expected = new Module(definitions=Map((Symbol("g") -> Global("g", UnitType, None))))
    assertEquals(Left(expected), parse(program, "<test>"))
  }
}

class ModuleIOWriteBinaryCollectTest {
  def makeWriter(program: String): BinaryModuleWriter = {
    val Left(module) = readText(program)
    val output = new DataOutputStream(new ByteArrayOutputStream)
    new BinaryModuleWriter(module, output)
  }

  def testCollect[T](program: String,
                     getTable: BinaryModuleWriter => BinaryModuleWriter#Table[T],
                     value: T)
  {
    val writer = makeWriter(program)
    writer.collect
    val table = getTable(writer)
    val index = table(value)
    assertEquals(value, table.get(index))
  }

  @Test
  def tableTest {
    val writer = makeWriter("")
    val table = writer.strings
    table.add("test")
    assertEquals(0, table("test"))
    assertEquals("test", table.get(0))
  }

  @Test
  def collectSymbolCollectsStrings {
    val writer = makeWriter("")
    val sym = symbolFromString("foo.bar")
    writer.collectSymbol(sym)
    assertEquals(sym.name(0), writer.strings.get(writer.strings(sym.name(0))))
  }

  @Test
  def collectDefinitionNameStrings {
    val program = "#global foo.bar: #unit"
    testCollect(program, _.strings, "foo")
    testCollect(program, _.strings, "bar")
  }

  @Test
  def collectDefinitionNames {
    val program = "#global foo.bar#32: #unit"
    testCollect(program, _.symbols, symbolFromString("foo.bar#32"))
  }
}

class ModuleIOWriteBinaryTest {
  val Left(module) = readText("")
  val output = new ByteArrayOutputStream
  val writer = new BinaryModuleWriter(module, new DataOutputStream(output))

  def testOutput(expected: Any*) {
    testOutput(expected.toList)
  }

  def testOutput(expected: List[Any]) {
    val expectedOutput = new ByteArrayOutputStream
    val stream = new DataOutputStream(expectedOutput)
    expected.foreach { 
      case b: Byte => stream.writeByte(b)
      case s: Short => stream.writeShort(s)
      case i: Int => stream.writeInt(i)
      case l: Long => stream.writeLong(l)
      case f: Float => stream.writeFloat(f)
      case d: Double => stream.writeDouble(d)
      case s: String => stream.writeUTF(s)
    }
    val data = output.toByteArray
    val expectedData = expectedOutput.toByteArray
    assertArrayEquals(expectedData, data)
  }

  @Test
  def testWriteOptionSome {
    writer.writeOption(Some(12), writer.writeInt _)
    testOutput(1.asInstanceOf[Byte], 12)
  }

  @Test
  def testWriteOptionNone {
    writer.writeOption(None, writer.writeInt _)
    testOutput(0.asInstanceOf[Byte])
  }

  @Test
  def testWriteList {
    writer.writeList(List(1, 2, 3), writer.writeInt _)
    testOutput(3, 1, 2, 3)
  }

  @Test
  def testWriteString {
    val s = "hello"
    writer.writeString(s)
    testOutput(s)
  }

  @Test
  def testWriteSymbol {
    writer.strings.add("foo")
    writer.strings.add("bar")
    writer.writeSymbol("foo.bar#32")
    testOutput(2, writer.strings("foo"), writer.strings("bar"), 32)
  }
}

class ModuleIOWriteTextTest {
  def parentPrefixes(parent: Symbol): List[Symbol] = {
    (List[Symbol]() /: parent.name) { (prefixes: List[Symbol], part: String) =>
      prefixes match {
        case Nil => List(Symbol(part, parent.id))
        case h :: _ => Symbol(h.name :+ part, parent.id) :: prefixes
      }
    }.reverse
  }

  def testWriteDefinitionText(expected: String, 
                              program: String,
                              symbol: Symbol,
                              parent: Option[Symbol] = None)
  {
    val Left(module) = ModuleIO.readText(program)
    val defn = module.definitions(symbol)
    val output = new StringWriter
    val writer = new TextModuleWriter(module, output)
    val prefixes = parent.toList.flatMap(parentPrefixes _)
    prefixes.foreach(writer.parentNames.push _)
    writer.writeDefinition(defn)

    assertEquals(expected, output.toString)
  }

  @Test
  def isTopLevelTest {
    assertFalse(isTopLevel(Block("a", Nil, Nil)))
    assertFalse(isTopLevel(Field("a", UnitType)))
    assertTrue(isTopLevel(Function("a", Nil, UnitType, Nil)))
    assertTrue(isTopLevel(Global("a", UnitType, None)))
    assertFalse(isTopLevel(ReturnInstruction("a", UnitValue)))
    assertFalse(isTopLevel(Parameter("a", UnitType)))
    assertTrue(isTopLevel(Struct("a", Nil)))
  }

  @Test
  def localSymbolTest {
    val module = new Module
    val output = new StringWriter
    val writer = new TextModuleWriter(module, output)

    def testLocalSymbol(expected: Symbol, parent: Symbol, name: Symbol) {
      val prefixes = parentPrefixes(parent)
      prefixes.foreach(writer.parentNames.push _)
      val localSym = writer.localSymbol(name)
      prefixes.foreach { _ => writer.parentNames.pop _ }
      assertEquals(expected, localSym)
    }

    testLocalSymbol("a", "main.entry", "main.entry.a")
    testLocalSymbol("foo", "main.entry", "main.foo")
    testLocalSymbol("foo", "main.entry.a", "main.foo")
    testLocalSymbol("bar.b", "foo.a", "bar.b")
    testLocalSymbol("a", "foo.a", "foo.a")
    testLocalSymbol("b3", "main.b2", "main.b3")
  }

  @Test
  def writeBlockText {
    val blockText = "  #block a(b: #unit, c: #unit) {\n" +
                    "    #return d = ()\n" +
                    "  }\n"
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  blockText +
                  "}\n"
    testWriteDefinitionText(blockText, program, "main.a", Some("main"))
  }

  @Test
  def writeStructTest {
    val structText = "#struct A {\n" +
                     "  #field x: #unit\n" +
                     "  #field y: #unit\n" +
                     "}\n"
    val program = structText +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"
    testWriteDefinitionText(structText, program, "A")
  }

  @Test
  def writeFunctionTest {
      val functionText = "#function foo(a: #unit, b: #unit): #unit {\n" +
                         "  #block entry( ) {\n" +
                         "    #return x = ()\n" +
                         "  }\n" +
                         "  #block other( ) {\n" +
                         "    #return x = ()\n" +
                         "  }\n" +
                         "}\n"
      val program = functionText +
                    "#function main( ): #unit {\n" +
                    "  #block entry( ) {\n" +
                    "    #return ()\n" +
                    "  }\n" +
                    "}\n"
    testWriteDefinitionText(functionText, program, "foo")
  }

  @Test
  def writeGlobalTest {
    val globalText = "#global a: #unit = ()\n"
    val program = globalText +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"
    testWriteDefinitionText(globalText, program, "a")
  }

  @Test
  def writeInstructionsTest {
    val stackText        = "    #stack stack: [1 * [1 * #unit]]*\n"
    val addressText      = "    #address address = stack, 0L, 0L\n"
    val assignText       = "    #assign assign = ()\n"
    val binopText        = "    #binop binop = 1 + 1\n"
    val branch1Text      = "    #branch branch1 = b1(1, 2)\n"
    val branch2Text      = "    #branch branch2 = b2( )\n"
    val condText         = "    #cond cond = #true ? b3(1, 2) : b4( )\n"
    val fextendText      = "    #fextend fextend = 3.0f : #float64\n"
    val ftoiText         = "    #ftoi ftoi = 3.0 : #int64\n"
    val ftruncateText    = "    #ftruncate ftruncate = 3.0 : #float32\n"
    val heapText         = "    #heap heap: #unit*\n"
    val heapArrayText    = "    #heaparray heaparray = 12L * #unit\n"
//    val icallText        = "    #icall icall = main( )\n"
    val isextendText     = "    #isextend isextend = -1 : #int64\n"
    val itofText         = "    #itof itof = 3 : #float64\n"
    val itruncateText    = "    #itruncate itruncate = 3L : #int8\n"
    val izextendText     = "    #izextend izextend = 12 : #int64\n"
    val loadText         = "    #load load = *l\n"
    val loadElementText  = "    #loadelement loadelement = [#unit: ()], 0L\n"
    val relopText        = "    #relop relop = 1 < 2\n"
    val returnText       = "    #return return = ()\n"
    val stackArrayText   = "    #stackarray stackarray = 1L * #unit\n"
    val scallText        = "    #scall scall = main( )\n"
    val storeText        = "    #store store = *l <- ()\n"
    val storeElementText = "    #storeelement storeelement = [#unit: ()], 0L <- ()\n"
    val upcastText       = "    #upcast upcast = #null: #unit*\n"
    val program = "#is64bit #true\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  stackText +
                  addressText +
                  assignText +
                  binopText +
                  branch1Text +
                  "  }\n" +
                  "  #block b1(a: #int32, b: #int32) {\n" +
                  branch2Text +
                  "  }\n" +
                  "  #block b2( ) {\n" +
                  condText +
                  "  }\n" +
                  "  #block b3(a: #int32, b: #int32) {\n" +
//                  icallText +
                  "    #stack l: #unit*\n" +
                  fextendText +
                  ftoiText +
                  ftruncateText +
                  heapText +
                  heapArrayText +
                  isextendText +
                  itofText +
                  itruncateText +
                  izextendText +
                  loadText +
                  loadElementText +
                  relopText +
                  stackArrayText +
                  scallText +
                  storeText +
                  storeElementText +
                  upcastText +
                  returnText + 
                  "  }\n" +
                  "  #block b4( ) {\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"

    testWriteDefinitionText(stackText, program, "main.entry.stack", Some("main.entry"))
    testWriteDefinitionText(addressText, program, "main.entry.address", Some("main.entry"))
    testWriteDefinitionText(assignText, program, "main.entry.assign", Some("main.entry"))
    testWriteDefinitionText(binopText, program, "main.entry.binop", Some("main.entry"))
    testWriteDefinitionText(branch1Text, program, "main.entry.branch1", Some("main.entry"))
    testWriteDefinitionText(branch2Text, program, "main.b1.branch2", Some("main.b1"))
    testWriteDefinitionText(condText, program, "main.b2.cond", Some("main.b2"))
    testWriteDefinitionText(fextendText, program, "main.b3.fextend", Some("main.b3"))
    testWriteDefinitionText(ftoiText, program, "main.b3.ftoi", Some("main.b3"))
    testWriteDefinitionText(ftruncateText, program, "main.b3.ftruncate", Some("main.b3"))
    testWriteDefinitionText(heapText, program, "main.b3.heap", Some("main.b3"))
    testWriteDefinitionText(heapArrayText, program, "main.b3.heaparray", Some("main.b3"))
    testWriteDefinitionText(isextendText, program, "main.b3.isextend", Some("main.b3"))
    testWriteDefinitionText(itofText, program, "main.b3.itof", Some("main.b3"))
    testWriteDefinitionText(itruncateText, program, "main.b3.itruncate", Some("main.b3"))
    testWriteDefinitionText(izextendText, program, "main.b3.izextend", Some("main.b3"))
//    testWriteDefinitionText(icallText, program, "main.b3.icall", Some("main.b3"))
    testWriteDefinitionText(loadText, program, "main.b3.load", Some("main.b3"))
    testWriteDefinitionText(loadElementText, program, "main.b3.loadelement", Some("main.b3"))
    testWriteDefinitionText(relopText, program, "main.b3.relop", Some("main.b3"))
    testWriteDefinitionText(returnText, program, "main.b3.return", Some("main.b3"))
    testWriteDefinitionText(stackArrayText, program, "main.b3.stackarray", Some("main.b3"))
    testWriteDefinitionText(scallText, program, "main.b3.scall", Some("main.b3"))
    testWriteDefinitionText(storeText, program, "main.b3.store", Some("main.b3"))
    testWriteDefinitionText(storeElementText, program, "main.b3.storeelement", Some("main.b3"))
    testWriteDefinitionText(upcastText, program, "main.b3.upcast", Some("main.b3"))
  }
}
