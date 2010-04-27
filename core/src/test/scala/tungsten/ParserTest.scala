package tungsten

import org.junit.Test
import org.junit.Assert._
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
    testType("[12 x unit]", ArrayType(Some(12), UnitType))
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
    testValue("unit %x", DefinedValue("%x"))
  }

  @Test
  def annotationValue {
    test("@x", Parser.annotationValue, AnnotationValue("@x", Nil))
    test("@x(true)", Parser.annotationValue, AnnotationValue("@x", List(BooleanValue(true))))
  }

  @Test
  def instName {
    test("@ann(()) assign %x =", Parser.instName("assign"), 
         Parser.~(List(AnnotationValue("@ann", List(UnitValue))), Symbol("%x")))
  }

  @Test
  def addressInst {
    testInstruction("address %x = (), ()",
                    AddressInstruction("%x", UnitValue, List(UnitValue)))
  }

  @Test
  def assignInst {
    testInstruction("assign %x = ()",
                    AssignInstruction("%x", UnitValue))
  }

  @Test
  def binopInst {
    testInstruction("binop %x = () % ()",
                    BinaryOperatorInstruction("%x", BinaryOperator.REMAINDER,
                                              UnitValue, UnitValue))
  }

  @Test
  def branchInst {
    testInstruction("branch %x = %a(())",
                    BranchInstruction("%x", "%a", List(UnitValue)))
  }

  @Test
  def condInst {
    testInstruction("cond %x = true ? %a(()) : %b(())",
                    ConditionalBranchInstruction("%x",
                                                 BooleanValue(true),
                                                 "%a",
                                                 List(UnitValue),
                                                 "%b",
                                                 List(UnitValue)))
  }

  @Test
  def fextendInst {
    testInstruction("fextend %x = () to unit",
                    FloatExtendInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def ftoiInst {
    testInstruction("ftoi %x = () to unit",
                    FloatToIntegerInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def ftruncateInst {
    testInstruction("ftruncate %x = () to unit",
                    FloatTruncateInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def heapInst {
    testInstruction("heap %x = unit",
                    HeapAllocateInstruction("%x", UnitType))
  }

  @Test
  def heapArrayInst {
    testInstruction("heaparray %x = () * unit",
                    HeapAllocateArrayInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def itofInst {
    testInstruction("itof %x = () to unit",
                    IntegerToFloatInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def isextendInst {
    testInstruction("isextend %x = () to unit",
                    IntegerSignExtendInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def itruncateInst {
    testInstruction("itruncate %x = () to unit",
                    IntegerTruncateInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def izextendInst {
    testInstruction("izextend %x = () to unit",
                    IntegerZeroExtendInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def intrinsicInst {
    testInstruction("intrinsic %x = exit(())",
                    IntrinsicCallInstruction("%x", Intrinsic.EXIT, List(UnitValue)))
  }

  @Test
  def loadInst {
    testInstruction("load %x = ()",
                    LoadInstruction("%x", UnitValue))
  }

  @Test
  def loadElementInst {
    testInstruction("loadelement %x = (), ()",
                    LoadElementInstruction("%x", UnitValue, List(UnitValue)))
  }

  @Test
  def relopInst {
    testInstruction("relop %x = () < ()",
                    RelationalOperatorInstruction("%x",
                                                  RelationalOperator.LESS_THAN,
                                                  UnitValue, UnitValue))
  }

  @Test
  def returnInst {
    testInstruction("return %x = ()",
                    ReturnInstruction("%x", UnitValue))
  }

  @Test
  def storeInst {
    testInstruction("store %x = (), ()",
                    StoreInstruction("%x", UnitValue, UnitValue))
  }

  @Test
  def storeElementInst {
    testInstruction("storeelement %x = (), (), ()",
                    StoreElementInstruction("%x", UnitValue, List(UnitValue), UnitValue))
  }

  @Test
  def stackInst {
    testInstruction("stack %x = unit",
                    StackAllocateInstruction("%x", UnitType))
  }

  @Test
  def stackArrayInst {
    testInstruction("stackarray %x = () * unit",
                    StackAllocateArrayInstruction("%x", UnitValue, UnitType))
  }

  @Test
  def staticCallInst {
    testInstruction("scall %x = @f(())",
                    StaticCallInstruction("%x", "@f", List(UnitValue)))
  }

  @Test
  def upcastInst {
    testInstruction("upcast %x = () to unit",
                    UpcastInstruction("%x", UnitValue, UnitType))
  }
}
