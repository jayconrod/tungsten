package tungsten

import org.junit.Test
import org.junit.Assert._

class AstLexerTest {
  @Test
  def empty = {
    val scanner = new AstLexer.Scanner("")
    assertTrue(scanner.atEnd)
  }

  @Test
  def emptyError = {
    val token = AstLexer.test("")
    assertTrue(token.isInstanceOf[ErrorToken])
  }

  @Test
  def whitespace = {
    val scanner = new AstLexer.Scanner(" \n\t")
    assertTrue(scanner.atEnd)
  }

  @Test
  def whitespaceError = {
    val token = AstLexer.test(" \n\t")
    assertTrue(token.isInstanceOf[ErrorToken])
  }

  @Test
  def unit = {
    val expected = ReservedToken("()")
    val token = AstLexer.test("()")
    assertEquals(expected, token)
    assertEquals(Location("<UNKNOWN>", 1, 1, 1, 2), token.location)
  }

  @Test
  def reserved = {
    for (r <- AstLexer.reservedStrings) {
      val expected = ReservedToken(r)
      val expectedLocation = Location("<UNKNOWN>", 1, 1, 1, r.length)
      val token = AstLexer.test(r)
      assertEquals(expected, token)
      assertEquals(expectedLocation, token.location)
    }
  }
}
