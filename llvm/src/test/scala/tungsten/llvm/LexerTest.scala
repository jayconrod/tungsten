/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import scala.util.parsing.input.CharArrayReader
import Lexer._

class LexerTest {
  def test[T](in: String, parser: Parser[T], expected: T) {
    val reader = new CharArrayReader(in.toArray)
    val result = phrase(parser)(reader).get
    assertEquals(expected, result)
  }

  def testToken(in: String, expected: Token) = {
    val scanner = new Scanner(in)
    val token = scanner.first
    assertEquals(expected, token)
    assertTrue(scanner.rest.atEnd)
  }

  def testTokens(in: String, expected: List[Token]) = {
    var reader = new Scanner(in)
    var tokens: List[Token] = Nil
    while (!reader.atEnd) {
      tokens = reader.first :: tokens
      reader = reader.rest
    }
    tokens = tokens.reverse
    assertEquals(expected, tokens)
  }

  def testTokenClass[T <: Token](in: String)(implicit m: Manifest[T]) = {
    val scanner = new Scanner(in)
    val token = scanner.first
    assertTrue(m.erasure.isInstance(token))
  }

  @Test
  def empty {
    val scanner = new Scanner("")
    assertTrue(scanner.atEnd)
  }

  @Test
  def emptyError {
    testTokenClass[ErrorToken]("")
  }

  @Test
  def whitespace {
    val scanner = new Scanner(" \n\t")
    assertTrue(scanner.atEnd)
  }

  @Test
  def whitespaceError {
    testTokenClass[ErrorToken](" \n\t")
  }

  @Test
  def comment {
    val scanner = new Scanner("; this is a comment\n" +
                              "  ; this is another comment\n\n")
    assertTrue(scanner.atEnd)
  }

  @Test
  def reserved {
    for (r <- reservedOperators ++ reservedWords) {
      val expected = ReservedToken(r)
      testToken(r, expected)
    }
  }

  @Test
  def string {
    val input = "\"this is a test\""
    val expected = StringToken(input)
    testToken(input, expected)
    assertEquals("this is a test", expected.value)
  }

  @Test
  def stringWithHex {
    val input = "\"this is\\0Aa multiline\\0A\\22test\\22\""
    val expectedToken = StringToken(input)
    val expectedStr = "this is\na multiline\n\"test\"" 
    testToken(input, expectedToken)
    assertEquals(expectedStr, expectedToken.value)
  }

  @Test
  def integers {
    testToken("0", IntToken("0"))
    testToken("123", IntToken("123"))
    testToken("-123", IntToken("-123"))
  }

  @Test
  def floats {
    testToken("1.0", FloatToken("1.0"))
    testToken("-2.0", FloatToken("-2.0"))
    testToken("1e4", FloatToken("1e4"))
  }

  @Test
  def intType {
    val input = "i32"
    val expected = IntTypeToken(input)
    testToken(input, expected)
    assertEquals(32, expected.width)
  }

  @Test
  def localSymbol {
    val input = "%x"
    val expected = SymbolToken(input)
    testToken(input, expected)
    assertEquals("%x", expected.value)
    assertFalse(expected.isGlobal)
  }

  @Test
  def globalSymbol {
    val input = "@foo.bar"
    val expected = SymbolToken(input)
    testToken(input, expected)
    assertEquals("@foo.bar", expected.value)
    assertTrue(expected.isGlobal)
  }

  @Test
  def quotedSymbol {
    val input = "%\"void\""
    val expected = SymbolToken(input)
    testToken(input, expected)
    assertEquals("%void", expected.value)
    assertFalse(expected.isGlobal)
  }

  @Test
  def escapedSymbol {
    val input = "%\"\\22\""
    val expected = SymbolToken(input)
    testToken(input, expected)
    assertEquals("%\"", expected.value)
  }

  @Test
  def numericSymbol {
    val input = "%0"
    val expected = SymbolToken(input)
    testToken(input, expected)
    assertEquals("%0", expected.value)
    assertFalse(expected.isGlobal)
  }

  @Test
  def label {
    val input = "bb0:"
    val expected = LabelToken("bb0")
    testToken(input, expected)
    assertEquals("%bb0", expected.value)
  }

  @Test
  def quotedLabel {
    testToken("\"bb 0\":", LabelToken("bb 0"))
  }

  @Test
  def labelConflict {
    testToken("return:", LabelToken("return"))
  }
}
