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
import Utilities._

class SymbolTest {
  @Test
  def createSimple {
    val sym = Symbol("simple")
    assertEquals(List("simple"), sym.name)
    assertEquals(0, sym.id)
  }

  @Test
  def createComplex {
    val name = List("not", "so", "simple")
    val sym = Symbol(name, 36)
    assertEquals(name, sym.name)
    assertEquals(36, sym.id)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def emptyName {
    val sym = Symbol(Nil, 0)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def emptyNamePart {
    val sym = Symbol(List("foo", "", "bar"), 0)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def negativeId {
    val id = Symbol(List("foo"), -32)
    ()
  }

  @Test
  def equals {
    val sym1 = Symbol("foo", 2)
    val sym2 = Symbol("foo", 2)
    assertTrue(sym1 == sym2)
  }

  @Test
  def notEquals {
    val sym1 = Symbol("foo", 2)
    val sym2 = Symbol("bar", 3)
    assertFalse(sym1 == sym2)
  }

  @Test
  def hash {
    val sym1 = Symbol("foo", 2)
    val sym2 = Symbol("foo", 2)
    assertEquals(sym1.hashCode, sym2.hashCode)
  }

  @Test
  def factory {
    val factory = new SymbolFactory
    val sym1 = factory.symbol("foo")
    val sym2 = factory.symbol("foo")
    assertTrue(sym1.id != sym2.id)
  }

  @Test
  def strings {
    val sym1 = Symbol(List("foo", "bar", "baz"), 33)
    assertEquals("foo.bar.baz#33", sym1.toString)
    val sym2 = Symbol(List("foo", "bar", "baz"), 0)
    assertEquals("foo.bar.baz", sym2.toString)
  }

  @Test
  def quoted {
    val sym = Symbol("multi word")
    assertEquals("\"multi word\"", sym.toString)
  }

  @Test
  def escaped {
    val sym = Symbol("multi\nline")
    assertEquals("\"multi\\000aline\"", sym.toString)
  }

  @Test
  def concat {
    val base = Symbol("foo")
    val expected = Symbol(List("foo", "bar"))
    val result = base + "bar"
    assertEquals(expected, result)
  }

  @Test
  def createImplicit {
    import Symbol._
    val sym: Symbol = "this.is.a.test#12"
    assertEquals(Symbol(List("this", "is", "a", "test"), 12), sym)
  }

  @Test
  def parent {
    assertEquals(Symbol("a"), Symbol(List("a", "b"), 1).parent)
  }

  @Test
  def withoutPrefix {
    val abcd = symbolFromString("a.b.c.d#12")
    val ab = symbolFromString("a.b#34")
    val cd = symbolFromString("c.d#12")
    val xy = symbolFromString("x.y#56")
    val axy = symbolFromString("a.x.y#78")
    assertEquals(None, abcd.withoutPrefix(abcd))
    assertEquals(None, abcd.withoutPrefix(xy))
    assertEquals(None, abcd.withoutPrefix(axy))
    assertEquals(None, abcd.withoutPrefix(cd))
    assertEquals(Some(cd), abcd.withoutPrefix(ab))
  }

  @Test
  def compare {
    assertEquals(0, Symbol("a").compare(Symbol("a")))
    assertTrue(Symbol("a").compare(Symbol("b")) < 0)
    assertTrue(Symbol("b").compare(Symbol("a")) > 0)
    assertTrue(symbolFromString("a.a").compare(Symbol("a")) > 0)
    assertTrue(Symbol("a").compare(Symbol("a.a")) < 0)
    assertEquals(0, symbolFromString("a.a").compare(symbolFromString("a.a")))
  }

  @Test(expected=classOf[IllegalStateException])
  def factoryOverflow {
    val factory = new SymbolFactory(Integer.MAX_VALUE)
    factory.symbol("foo")
  }
}
