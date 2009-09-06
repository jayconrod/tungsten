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

  @Test
  def empty = {
    testModule("", AstModule(Nil))
  }

  @Test
  def whitespace = {
    testModule(" \t\n", AstModule(Nil))
  }

  @Test
  def global = {
    val expected = AstModule(List(AstGlobal(new Symbol("foo"), AstUnitType(), None)))
    testModule("#global foo: #unit", expected)
  }
}
