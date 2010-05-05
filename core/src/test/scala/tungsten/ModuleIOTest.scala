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
    val program = "global unit @foo.bar"
    testCollect(program, _.strings, "foo")
    testCollect(program, _.strings, "bar")
  }

  @Test
  def collectDefinitionNames {
    val program = "global unit @foo.bar#32"
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
  val output = new StringWriter
  val emptyModule = new Module(is64Bit=true)
  val dummyWriter = new ModuleIO.TextModuleWriter(emptyModule, output)


  @Test
  def values {    
    assertEquals("()", dummyWriter.localValue(UnitValue, None))
    assertEquals("true", dummyWriter.localValue(BooleanValue(true), None))
    assertEquals("false", dummyWriter.localValue(BooleanValue(false), None))
    assertEquals("'c'", dummyWriter.localValue(CharValue('c'), None))
    assertEquals("'\\000a'", dummyWriter.localValue(CharValue('\n'), None))
    assertEquals("\"hello\"", dummyWriter.localValue(StringValue("hello"), None))
    assertEquals("\"multi\\000aline\"", dummyWriter.localValue(StringValue("multi\nline"), None))
    assertEquals("int32 12", dummyWriter.localValue(IntValue(12L, 32), None))
    assertEquals("[1 x unit] {()}", dummyWriter.localValue(ArrayValue(UnitType, List(UnitValue)), None))
    assertEquals("struct @A {()}", dummyWriter.localValue(StructValue("A", List(UnitValue)), None))
  }

  @Test
  def types {
    assertEquals("unit", dummyWriter.localType(UnitType, None))
    assertEquals("boolean", dummyWriter.localType(BooleanType, None))
    assertEquals("char", dummyWriter.localType(CharType, None))
    assertEquals("string", dummyWriter.localType(StringType, None))
    assertEquals("int32", dummyWriter.localType(IntType(32), None))
    assertEquals("float32", dummyWriter.localType(FloatType(32), None))
    assertEquals("unit*", dummyWriter.localType(PointerType(UnitType), None))
    assertEquals("nulltype", dummyWriter.localType(NullType, None))
    assertEquals("[? x unit]", dummyWriter.localType(ArrayType(None, UnitType), None))
    assertEquals("[2 x unit]", dummyWriter.localType(ArrayType(Some(2L), UnitType), None))
    assertEquals("struct @A", dummyWriter.localType(StructType("A"), None))
  }

  @Test
  def localSymbol {
    assertEquals("@a", dummyWriter.localSymbol("a", None).toString)
    assertEquals("@b.c", dummyWriter.localSymbol("b.c", Some("a")).toString)
    assertEquals("%b", dummyWriter.localSymbol("a.b", Some("a")).toString)
  }

  @Test
  def localType {
    val ty = StructType("B.A")
    assertEquals("struct %A", dummyWriter.localType(ty, Some("B")))
  }

  @Test
  def localValue {
    val value = DefinedValue("b.a", StructType("A"))
    assertEquals("struct @A %a", dummyWriter.localValue(value, Some("b")))
  }

  @Test
  def writeChildren {
    val children = List(1, 2, 3)
    def writer(i: Int) = output.write(i.toString)
    dummyWriter.writeChildren(children, writer, "(", ", ", ")")
    assertEquals("(1, 2, 3)", output.toString)
  }
}
