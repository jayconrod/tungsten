package tungsten

import org.junit.Test
import org.junit.Assert._
import scala.util.parsing.input.CharArrayReader

class AstLexerTest {
  def test[T](in: String, parser: AstLexer.Parser[T], expected: T) = {
    val reader = new CharArrayReader(in.toArray)
    val result = AstLexer.phrase(parser)(reader).get
    assertEquals(expected, result)
  }

  def testToken(in: String, expected: Token) = {
    val scanner = new AstLexer.Scanner(in)
    val token = scanner.first
    assertEquals(expected, token)
    assertTrue(scanner.rest.atEnd)
  }

  def testTokens(in: String, expected: List[Token]) = {
    var reader = new AstLexer.Scanner(in)
    var tokens: List[Token] = Nil
    while (!reader.atEnd) {
      tokens = reader.first :: tokens
      reader = reader.rest
    }
    tokens = tokens.reverse
    assertEquals(expected, tokens)
  }

  @Test
  def empty = {
    val scanner = new AstLexer.Scanner("")
    assertTrue(scanner.atEnd)
  }

  @Test
  def emptyError = {
    testToken("", ErrorToken(""))
  }

  @Test
  def whitespace = {
    val scanner = new AstLexer.Scanner(" \n\t")
    assertTrue(scanner.atEnd)
  }

  @Test
  def whitespaceError = {
     testToken(" \n\t", ErrorToken(""))
  }

  @Test
  def reserved = {
    for (r <- AstLexer.reservedStrings) {
      val expected = ReservedToken(r)
      testToken(r, expected)
    }
  }

  @Test
  def integer = {
    test("12b", AstLexer.byte, 12: Byte)
    test("12s", AstLexer.short, 12: Short)
    test("12", AstLexer.int, 12)
    test("0", AstLexer.int, 0)
    test("10000000000L", AstLexer.long, 10000000000L)
  }

  @Test
  def integerToken = {
    testToken("12b", ByteToken(12))
    testToken("12s", ShortToken(12))
    testToken("12", IntToken(12))
    testToken("12L", LongToken(12))
  }

  @Test
  def identifier = {
    test("_foo0", AstLexer.identifier, "_foo0")
  }

  @Test(expected=classOf[RuntimeException])
  def identifierStartingWithDigit = {
    test("1foo", AstLexer.identifier, "")
  }

  @Test
  def symbol = {
    test("foo.bar.baz", AstLexer.symbol, Symbol(List("foo", "bar", "baz"), 0))
  }

  @Test
  def symbolWithId = {
    test("foo#12", AstLexer.symbol, Symbol(List("foo"), 12))
  }

  @Test(expected=classOf[RuntimeException])
  def symbolWithLargeId = {
    test("foo#10000000000", AstLexer.symbol, Symbol(List("foo"), 0))
  }

  @Test
  def symbolToken = {
    testToken("foo.bar#12", SymbolToken(Symbol(List("foo", "bar"), 12)))
  }

  @Test
  def location = {
    test("<foo/bar.w:12.34-56.78>", AstLexer.location, Location("foo/bar.w", 12, 34, 56, 78))
  }

  @Test
  def functionTokens = {
    val program = "#function"
    val tokens = List(ReservedToken("#function"))
    testTokens(program, tokens)
  }
}
