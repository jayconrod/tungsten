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
}
