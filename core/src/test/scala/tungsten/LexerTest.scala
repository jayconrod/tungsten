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

  def testFailure(in: String) {
    val reader = new CharArrayReader(in.toArray)
    val result = Lexer.phrase(Lexer.token)(reader)
    assertTrue(result.isInstanceOf[Lexer.NoSuccess])
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

  @Test
  def chars {
    testToken("'a'", CharTok('a'))
    testToken("'\\41'", CharTok('A'))
    testToken("'\\0041'", CharTok('A'))
    testToken("'\"'", CharTok('"'))
    testFailure("'''")
  }

  @Test
  def strings {
    testToken("\"hello\"", StringTok("hello"))
    testToken("\"multi\\0aline\"", StringTok("multi\nline"))
    testToken("\"single'quote\"", StringTok("single'quote"))
  }

  @Test
  def symbols {
    testToken("@x", SymbolTok(Symbol("@x")))
    testToken("%foo", SymbolTok(Symbol("%foo")))
    testToken("%x.y.z", SymbolTok(Symbol(List("%x", "y", "z"))))
    testToken("%x#12", SymbolTok(Symbol("%x", 12)))
    testToken("%\"x y\"#12", SymbolTok(Symbol("%x y", 12)))
    testToken("%\"multi\\0Aline\"", SymbolTok(Symbol("%multi\nline")))
    testToken("%\"multi\\000aline\"", SymbolTok(Symbol("%multi\nline")))
  }
}
