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
  def commentWhitespace {
    val scanner = new Lexer.Scanner("; this is a comment")
    assertTrue(scanner.atEnd)
  }

  @Test
  def integers {
    testToken("0", IntTok(0L))
    testToken("-0", IntTok(0L))
    testToken("12", IntTok(12L))
    testToken("-12", IntTok(-12L))
  }

  @Test
  def floats {
    testToken("1.", FloatTok(1.))
    testToken("-1.", FloatTok(-1.))
    testToken("+1.", FloatTok(+1.))
    testToken("1.5", FloatTok(1.5))
    testToken("1.5e2", FloatTok(1.5e2))
    testToken("1.5E2", FloatTok(1.5E2))
    testToken("1.5e-2", FloatTok(1.5e-2))
    testToken("1.5e+2", FloatTok(1.5e+2))
    testToken(".5", FloatTok(.5))
    testToken("-.5", FloatTok(-.5))
    testToken(".5e2", FloatTok(.5e2))
    testToken("1e2", FloatTok(1e2))
    testToken("-1e2", FloatTok(-1e2))
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

  @Test
  def reserved {
    for (r <- Lexer.reservedStrings)
      testToken(r, ReservedTok(r))
  }

  @Test
  def version {
    testToken("v1", VersionTok(Version(1)))
    testToken("v12.34", VersionTok(Version(12, 34)))
  }

  @Test
  def moduleDependency {
    testToken("-lfoo", ModuleDependencyTok(ModuleDependency("foo", Version.MIN, Version.MAX)))
    testToken("-lbar:0.1-", ModuleDependencyTok(ModuleDependency("bar", Version(0, 1), Version.MAX)))
    testToken("-lbaz:-1.0", ModuleDependencyTok(ModuleDependency("baz", Version.MIN, Version(1, 0))))
  }
}
