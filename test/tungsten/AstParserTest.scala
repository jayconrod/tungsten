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

  @Test
  def empty = {
    testModule("", AstModule(Nil))
  }

  @Test
  def whitespace = {
    testModule(" \t\n", AstModule(Nil))
  }

  @Test
  def typeWithLocation = {
    testType("#unit <foo.w>:1.2-3.4", AstUnitType(Location("foo.w", 1, 2, 3, 4)))
  }

  @Test
  def value = {
    testValue("()", AstUnitValue(Nowhere))
  }

  @Test
  def valueWithLocation = {
    val location = Location("foo.w", 1, 2, 3, 4)
    val expected = AstUnitValue(location)
    testValue("() <foo.w>:1.2-3.4", expected)
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
    testModule("#global <foo.w>:1.2-3.4 foo: #unit", expected)
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
