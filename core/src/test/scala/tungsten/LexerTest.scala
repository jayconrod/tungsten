package tungsten

import org.junit.Test
import org.junit.Assert._
import scala.util.parsing.input.CharArrayReader
import Utilities._

class LexerTest {
  def test[T](in: String, parser: Lexer.Parser[T], expected: T) {
    val reader = new CharArrayReader(in.toArray)
    val result = Lexer.phrase(parser)(reader).get
    assertEquals(expected, result)
  }

  def testToken(in: String, expected: Lexer.Token) {
    val scanner = new Lexer.Scanner(in)
    val token = scanner.first
    assertEquals(expected, token)
    assertTrue(scanner.rest.atEnd)
  }

  @Test
  def empty {
    val scanner = new Lexer.Scanner("")
    assertTrue(scanner.atEnd)
  }

  @Test
  def emptyError {
    testToken("", ErrorTok(""))
  }

  @Test
  def whitespace {
    val scanner = new Lexer.Scanner(" \n\t")
    assertTrue(scanner.atEnd)
  }

  @Test
  def whitespaceError {
    testToken(" \n\t", ErrorTok(""))
  }

  @Test
  def integers {
    testToken("0", IntTok(0L))
    testToken("-0", IntTok(0L))
    testToken("12", IntTok(12L))
    testToken("-12", IntTok(-12L))
  }
}
