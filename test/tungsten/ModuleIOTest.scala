package tungsten

import org.junit.Test
import org.junit.Assert._
import java.io._
import ModuleIO._

class ModuleIOTest {
  /* writeBinary tests */
  def testWriteDefinitionText(expected: String, 
                              program: String,
                              symbol: Symbol,
                              parent: Option[Symbol] = None)
  {
    val Left(module) = ModuleIO.readText(program)
    val defn = module.definitions(symbol)
    val output = new StringWriter
    val writer = new TextModuleWriter(module, output)
    parent foreach { p => writer.parentNames.push(p) }
    writer.writeDefinition(defn)
    if (parent.isDefined)
      writer.parentNames.pop

    if (expected != output.toString) {
      System.err.println(expected)
      System.err.println(output.toString)
    }
    assertEquals(expected, output.toString)
  }

  @Test
  def isTopLevelTest {
    assertFalse(isTopLevel(Block("a", Nil, Nil)))
    assertFalse(isTopLevel(Field("a", UnitType())))
    assertTrue(isTopLevel(Function("a", Nil, UnitType(), Nil)))
    assertTrue(isTopLevel(Global("a", UnitType(), None)))
    assertFalse(isTopLevel(ReturnInstruction("a", UnitValue())))
    assertFalse(isTopLevel(Parameter("a", UnitType())))
    assertTrue(isTopLevel(Struct("a", Nil)))
  }

  @Test
  def locationStringTest {
    val loc = Location("foo.w", 1, 2, 3, 4)
    assertEquals(loc.toString + " ", locationString(loc))
    assertEquals("", locationString(Nowhere))
  }

  @Test
  def localSymbolTest {
    val module = new Module
    val output = new StringWriter
    val writer = new TextModuleWriter(module, output)

    def testLocalSymbol(expected: Symbol, parent: Symbol, name: Symbol) {
      writer.parentNames.push(parent)
      val localSym = writer.localSymbol(name)
      writer.parentNames.pop
      assertEquals(expected, localSym)
    }

    testLocalSymbol("a", "main.entry", "main.entry.a")
    testLocalSymbol("foo", "main.entry", "main.foo")
    testLocalSymbol("foo", "main.entry.a", "main.foo")
    testLocalSymbol("bar.b", "foo.a", "bar.b")
    testLocalSymbol("a", "foo.a", "foo.a")
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
    testWriteDefinitionText(blockText, program, "main.a")
  }

  @Test
  def writeStructTest {
    val structText = "#struct <foo.w:1.2-3.4> A {\n" +
                     "  #field <foo.w:5.6-7.8> x: #unit\n" +
                     "  #field <foo.w:9.10-11.12> y: #unit\n" +
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
      val functionText = "#function <foo.w:1.1-1.1> foo(a <foo.w:2.2-2.2> : #unit, b: #unit): #unit {\n" +
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
    val stackText        = "    #stack <foo.w:1.1-1.1> stack: [1 * [1 * #unit]]*\n"
    val addressText      = "    #address <foo.w:1.1-1.1> address = stack, 0L, 0L\n"
    val assignText       = "    #assign <foo.w:1.1-1.1> assign = ()\n"
    val binopText        = "    #binop <foo.w:1.1-1.1> binop = 1 + 1\n"
    val branch1Text      = "    #branch <foo.w:1.1-1.1> branch1 = b1(1, 2)\n"
    val branch2Text      = "    #branch <foo.w:1.1-1.1> branch2 = b2( )\n"
    val condText         = "    #cond <foo.w:1.1-1.1> cond = #true ? b3(1, 2) : b4( )\n"
//    val icallText        = "    #icall <foo.w:1.1-1.1> icall = main( )\n"
    val loadText         = "    #load <foo.w:1.1-1.1> load = *l\n"
    val loadElementText  = "    #loadelement <foo.w:1.1-1.1> loadelement = [#unit: ()], 0L\n"
    val relopText        = "    #relop <foo.w:1.1-1.1> relop = 1 < 2\n"
    val returnText       = "    #return return = ()\n"
    val stackArrayText   = "    #stackarray <foo.w:1.1-1.1> stackarray = 1L * #unit\n"
    val scallText        = "    #scall <foo.w:1.1-1.1> scall = main( )\n"
    val storeText        = "    #store <foo.w:1.1-1.1> store = *l <- ()\n"
    val storeElementText = "    #storeelement <foo.w:1.1-1.1> storeelement = [#unit: ()], 0L <- ()\n"
    val upcastText       = "    #upcast <foo.w:1.1-1.1> upcast = #null: #unit*\n"
    val program = "#function main( ): #unit {\n" +
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
