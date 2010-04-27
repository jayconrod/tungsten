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
}
