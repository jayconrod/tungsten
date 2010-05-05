package tungsten

import org.junit.Test
import org.junit.Assert._
import java.io.File
import Utilities._

class ParserTest {
  def test[T](input: String, parser: Parser.Parser[T], expected: T) {
    val scanner = new Lexer.Scanner(input)
    Parser.phrase(parser)(scanner) match {
      case Parser.Success(r, _) => assertEquals(expected, r)
      case error: Parser.NoSuccess => fail(error.msg)
    }
  }

  def testType(input: String, expected: Type) {
    test(input, Parser.ty, expected)
  }

  def testValue(input: String, expected: Value) {
    test(input, Parser.value, expected)
  }

  def testInstruction(input: String, expected: Instruction) {
    test(input, Parser.instruction, expected)
  }

  def testDefinition(input: String, 
                     parser: Parser.Parser[AstNode], 
                     expected: Definition) 
  {
    val scanner = new Lexer.Scanner(input)
    Parser.phrase(parser)(scanner) match {
      case Parser.Success(node, _) => assertEquals(expected, node.definition)
      case error: Parser.NoSuccess => fail(error.msg)
    }
  }

  @Test
  def reserved {
    test("{", Parser.reserved("{"), "{")
  }

  @Test
  def symbol {
    test("@x", Parser.symbol, Symbol("@x"))
  }

  @Test
  def integer {
    test("123", Parser.integer, 123L)
  }

  @Test
  def float {
    test("1.5", Parser.float, 1.5)
  }

  @Test
  def char {
    test("'a'", Parser.char, 'a')
  }

  @Test
  def string {
    test("\"hello\"", Parser.string, "hello")
  }

  @Test
  def unitType {
    testType("unit", UnitType)
  }

  @Test
  def booleanType {
    testType("boolean", BooleanType)
  }

  @Test
  def charType {
    testType("char", CharType)
  }

  @Test
  def stringType {
    testType("string", StringType)
  }

  @Test
  def intTypes {
    testType("int8", IntType(8))
    testType("int16", IntType(16))
    testType("int32", IntType(32))
    testType("int64", IntType(64))
  }

  @Test
  def floatTypes {
    testType("float32", FloatType(32))
    testType("float64", FloatType(64))
  }

  @Test
  def nullType {
    testType("nulltype", NullType)
  }

  @Test
  def structType {
    testType("struct @T", StructType(Symbol("@T")))
  }

  @Test
  def arrayTypes {
    testType("[? x unit]", ArrayType(None, UnitType))
    testType("[12 x unit]", ArrayType(Some(12L), UnitType))
  }

  @Test
  def pointerType {
    testType("unit**", PointerType(PointerType(UnitType)))
  }

  @Test
  def unitValue {
    testValue("()", UnitValue)
  }

  @Test
  def booleanValues {
    testValue("true", BooleanValue(true))
    testValue("false", BooleanValue(false))
  }

  @Test
  def charValue {
    testValue("'a'", CharValue('a'))
  }

  @Test
  def stringValue {
    testValue("\"hello\"", StringValue("hello"))
  }

  @Test
  def integerValues {
    testValue("int8 12", IntValue(12L, 8))
    testValue("int16 12", IntValue(12L, 16))
    testValue("int32 12", IntValue(12L, 32))
    testValue("int64 12", IntValue(12L, 64))
  }

  @Test
  def floatValues {
    testValue("float32 1.5", FloatValue(1.5, 32))
    testValue("float64 1.5", FloatValue(1.5, 64))
  }

  @Test
  def nullValue {
    testValue("null", NullValue)
  }

  @Test
  def arrayValue {
    testValue("[0 x unit] {}", ArrayValue(UnitType, Nil))
    testValue("[1 x unit] {()}", ArrayValue(UnitType, List(UnitValue)))
    testValue("[2 x unit] {(), ()}", ArrayValue(UnitType, List(UnitValue, UnitValue)))
  }

  @Test
  def structValue {
    testValue("struct @T {(), ()}", StructValue("@T", List(UnitValue, UnitValue)))
  }

  @Test
  def definedValue {
    testValue("unit %x", DefinedValue("%x", UnitType))
  }

  @Test
  def annotationValue {
    test("@x", Parser.annotationValue, AnnotationValue("@x", Nil))
    test("@x(true)", Parser.annotationValue, AnnotationValue("@x", List(BooleanValue(true))))
  }

  @Test
  def instName {
    val anns = List(AnnotationValue("@ann", List(UnitValue)))
    val ty = UnitType
    val n = Symbol("%x")
    test("@ann(()) assign unit %x =", Parser.instName("assign"), 
         Parser.~(Parser.~(anns, ty), n))
  }

  @Test
  def addressInst {
    testInstruction("address unit %x = (), ()",
                    AddressInstruction("%x", UnitType, UnitValue, List(UnitValue)))
  }

  @Test
  def assignInst {
    testInstruction("assign unit %x = ()",
                    AssignInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def binopInst {
    testInstruction("binop unit %x = () % ()",
                    BinaryOperatorInstruction("%x", UnitType, BinaryOperator.REMAINDER,
                                              UnitValue, UnitValue))
  }

  @Test
  def branchInst {
    testInstruction("branch unit %x = %a(())",
                    BranchInstruction("%x", UnitType, "%a", List(UnitValue)))
  }

  @Test
  def condInst {
    testInstruction("cond unit %x = true ? %a(()) : %b(())",
                    ConditionalBranchInstruction("%x",
                                                 UnitType,
                                                 BooleanValue(true),
                                                 "%a",
                                                 List(UnitValue),
                                                 "%b",
                                                 List(UnitValue)))
  }

  @Test
  def fextendInst {
    testInstruction("fextend unit %x = ()",
                    FloatExtendInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def ftoiInst {
    testInstruction("ftoi unit %x = ()",
                    FloatToIntegerInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def ftruncateInst {
    testInstruction("ftruncate unit %x = ()",
                    FloatTruncateInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def heapInst {
    testInstruction("heap unit %x",
                    HeapAllocateInstruction("%x", UnitType))
  }

  @Test
  def heapArrayInst {
    testInstruction("heaparray unit %x = () x unit",
                    HeapAllocateArrayInstruction("%x", UnitType, UnitValue, UnitType))
  }

  @Test
  def itofInst {
    testInstruction("itof unit %x = ()",
                    IntegerToFloatInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def isextendInst {
    testInstruction("isextend unit %x = ()",
                    IntegerSignExtendInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def itruncateInst {
    testInstruction("itruncate unit %x = ()",
                    IntegerTruncateInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def izextendInst {
    testInstruction("izextend unit %x = ()",
                    IntegerZeroExtendInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def intrinsicInst {
    testInstruction("intrinsic unit %x = exit(())",
                    IntrinsicCallInstruction("%x", UnitType, Intrinsic.EXIT, List(UnitValue)))
  }

  @Test
  def loadInst {
    testInstruction("load unit %x = ()",
                    LoadInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def loadElementInst {
    testInstruction("loadelement unit %x = (), ()",
                    LoadElementInstruction("%x", UnitType, UnitValue, List(UnitValue)))
  }

  @Test
  def relopInst {
    testInstruction("relop unit %x = () < ()",
                    RelationalOperatorInstruction("%x",
                                                  UnitType,
                                                  RelationalOperator.LESS_THAN,
                                                  UnitValue, UnitValue))
  }

  @Test
  def returnInst {
    testInstruction("return unit %x = ()",
                    ReturnInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def storeInst {
    testInstruction("store unit %x = (), ()",
                    StoreInstruction("%x", UnitType, UnitValue, UnitValue))
  }

  @Test
  def storeElementInst {
    testInstruction("storeelement unit %x = (), (), ()",
                    StoreElementInstruction("%x", UnitType, UnitValue, UnitValue, List(UnitValue)))
  }

  @Test
  def stackInst {
    testInstruction("stack unit %x",
                    StackAllocateInstruction("%x", UnitType))
  }

  @Test
  def stackArrayInst {
    testInstruction("stackarray unit %x = () x unit",
                    StackAllocateArrayInstruction("%x", UnitType, UnitValue, UnitType))
  }

  @Test
  def staticCallInst {
    testInstruction("scall unit %x = @f(())",
                    StaticCallInstruction("%x", UnitType, "@f", List(UnitValue)))
  }

  @Test
  def upcastInst {
    testInstruction("upcast unit %x = ()",
                    UpcastInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def parameter {
    testDefinition("@ann unit %p", Parser.parameter,
                   Parameter("%p", UnitType, List(AnnotationValue("@ann", Nil))))
  }

  @Test
  def block {
    testDefinition("block %b(unit %x) {\n" +
                   "  return unit %i = ()\n" +
                   "}", 
                   Parser.block,
                   Block("%b", List("%b.x"), List("%b.i")))
  }

  @Test
  def field {
    testDefinition("field unit %a", Parser.field,
                   Field("%a", UnitType))
  }

  @Test
  def annotation {
    testDefinition("annotation @ann(unit %a, unit %b)", Parser.annotation,
                   Annotation("@ann", List("@ann.a", "@ann.b")))
  }

  @Test
  def function {
    testDefinition("function unit @f(unit %a, unit %b) {\n" +
                   "  block %c() {\n" +
                   "    return ()\n" +
                   "  }\n" +
                   "  block %d() {\n" +
                   "    return ()\n" +
                   "  }\n" +
                   "}\n",
                   Parser.function,
                   Function("@f", UnitType, List("@f.a", "@f.b"), List("@f.c", "@f.d"), Nil))
  }

  @Test
  def global {
    testDefinition("global unit @g", Parser.global,
                   Global("@g", UnitType, None))
    testDefinition("global unit @g = ()", Parser.global,
                   Global("@g", UnitType, Some(UnitValue)))
  }

  @Test
  def struct {
    testDefinition("struct @s {\n" +
                   "  field unit %a\n" +
                   "  field unit %b\n" +
                   "}\n",
                   Parser.struct,
                   Struct("@s", List("@s.a", "@s.b")))
  }

  @Test
  def childNames {
    val child = Parameter("%p", UnitType)
    val childNodes = List(AstNode(child, Nil))
    assertEquals(List(Symbol(List("@a", "p"))), Parser.childNames(childNodes, "@a"))
  }

  @Test
  def emptyModule {
    test("", Parser.module,
         (new Module(), Nil))
  }

  @Test
  def headers {
    test("name: @m\n" +
         "type: library\n" +
         "version: v0.1\n" +
         "filename: \"foo.w\"\n" +
         "dependencies: -lfoo, -lbar:0.1-, -lbaz:-1.0\n" +
         "searchpaths: \"/foo/bar\", \"/baz\"\n" +
         "is64bit: true\n" +
         "safe: true\n",
         Parser.headers,
         new Module(name=Symbol("m"),
                    ty=ModuleType.LIBRARY,
                    version=Version(0, 1),
                    filename=Some(new File("foo.w")),
                    dependencies=List(ModuleDependency("foo", Version.MIN, Version.MAX),
                                      ModuleDependency("bar", Version(0, 1), Version.MAX),
                                      ModuleDependency("baz", Version.MIN, Version(1, 0))),
                    searchPaths=List(new File("/foo/bar"), new File("/baz")),
                    is64Bit=true,
                    isSafe=true))
  }
}

