package tungsten

import org.junit.Test
import org.junit.Assert._

class AstParserTest {
  def test[T](input: String, parser: AstParser.Parser[T], expected: T) = {
    val scanner = new AstLexer.Scanner(input)
    val result = AstParser.phrase(parser)(scanner).get
    assertEquals(expected, result)
  } 

  def testModule(input: String, expected: AstModule) = {
    test(input, AstParser.module, expected)
  }

  def testType(input: String, expected: AstType) = {
    test(input, AstParser.ty, expected)
  }

  def testValue(input: String, expected: AstValue) = {
    test(input, AstParser.value, expected)
  }

  def testInstruction(input: String, expected: AstInstruction) = {
    test(input, AstParser.instruction, expected)
  }

  @Test
  def empty = {
    testModule("", AstModule(Nil))
  }

  @Test
  def whitespace = {
    testModule(" \t\n", AstModule(Nil))
  }

  @Test
  def types = {
    testType("#unit", AstUnitType(Nowhere))
    testType("#int32", AstIntType(32, Nowhere))
  }

  @Test
  def typeWithLocation = {
    testType("#unit <foo.w:1.2-3.4>", AstUnitType(Location("foo.w", 1, 2, 3, 4)))
  }

  @Test
  def value = {
    testValue("()", AstUnitValue(Nowhere))
    testValue("123", AstIntValue(123, Nowhere))
    testValue("foo", AstSymbolValue(new Symbol("foo"), Nowhere))
  }

  @Test
  def valueWithLocation = {
    val location = Location("foo.w", 1, 2, 3, 4)
    val expected = AstUnitValue(location)
    testValue("() <foo.w:1.2-3.4>", expected)
  }

  @Test
  def instruction = {
    testInstruction("#return 123", AstReturnInstruction(AstIntValue(123, Nowhere), Nowhere))
  }

  @Test
  def parameter = {
    val loc = Location("foo.w", 1, 2, 3, 4)
    val tyLoc = Location("foo.w", 5, 6, 7, 8)
    val expected = AstParameter(new Symbol("foo"), AstIntType(32, tyLoc), loc)
    test("foo <foo.w:1.2-3.4> : #int32 <foo.w:5.6-7.8>", AstParser.parameter, expected)
  }

  @Test
  def block = {
    val program = "#block <foo.w:1.2-3.4> foo(bar : #int32,\n" +
                  "                           baz : #int32) {\n" +
                  "  #return 123\n" +
                  "}\n"
    val loc = Location("foo.w", 1, 2, 3, 4)
    val (p1, p2) = (AstParameter(new Symbol("bar"), new AstIntType(32, Nowhere), Nowhere),
                    AstParameter(new Symbol("baz"), new AstIntType(32, Nowhere), Nowhere))
    val expected = AstBlock(new Symbol("foo"),
                            List(p1, p2),
                            List(AstReturnInstruction(AstIntValue(123, Nowhere), Nowhere)),
                            loc)
    test(program, AstParser.block, expected)
  }

  @Test
  def global = {
    val global = AstGlobal(new Symbol("foo"), AstUnitType(Nowhere), None, Nowhere)
    val expected = AstModule(List(global))
    testModule("#global foo: #unit", expected)
  }

  @Test
  def globalWithLocation = {
    val loc = Location("foo.w", 1, 2, 3, 4)
    val global = AstGlobal(new Symbol("foo"), AstUnitType(Nowhere), None, loc)
    val expected = AstModule(List(global))
    testModule("#global <foo.w:1.2-3.4> foo: #unit", expected)
  }

  @Test
  def globalWithValue = {
    val global = AstGlobal(new Symbol("foo"), 
                           AstUnitType(Nowhere),
                           Some(AstUnitValue(Nowhere)),
                           Nowhere)
    val expected = AstModule(List(global))
    testModule("#global foo: #unit = ()", expected)
  }
}
