package tungsten

import org.junit.Test
import org.junit.Assert._

class SymbolTest {
  @Test
  def createSimple = {
    val sym = new Symbol("simple")
    assertEquals(List("simple"), sym.name)
    assertEquals(0, sym.id)
  }

  @Test
  def createComplex = {
    val name = List("not", "so", "simple")
    val sym = new Symbol(name, 36)
    assertEquals(name, sym.name)
    assertEquals(36, sym.id)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def emptyName = {
    val sym = new Symbol(Nil, 0)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def emptyNamePart = {
    val sym = new Symbol(List("foo", "", "bar"), 0)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def negativeId = {
    val id = new Symbol(List("foo"), -32)
    ()
  }

  @Test
  def equals = {
    val sym1 = new Symbol("foo", 2)
    val sym2 = new Symbol("foo", 2)
    assertTrue(sym1 == sym2)
  }

  @Test
  def notEquals = {
    val sym1 = new Symbol("foo", 2)
    val sym2 = new Symbol("bar", 3)
    assertFalse(sym1 == sym2)
  }

  @Test
  def hash = {
    val sym1 = new Symbol("foo", 2)
    val sym2 = new Symbol("foo", 2)
    assertEquals(sym1.hashCode, sym2.hashCode)
  }

  @Test
  def factory = {
    val factory = new SymbolFactory
    val sym1 = factory.symbol("foo")
    val sym2 = factory.symbol("foo")
    assertTrue(sym1.id != sym2.id)
  }

  @Test
  def strings = {
    val sym1 = new Symbol(List("foo", "bar", "baz"), 33)
    assertEquals("foo.bar.baz#33", sym1.toString)
    val sym2 = new Symbol(List("foo", "bar", "baz"), 0)
    assertEquals("foo.bar.baz", sym2.toString)
  }

  @Test
  def concat = {
    val base = new Symbol("foo")
    val expected = new Symbol(List("foo", "bar"))
    val result = base + "bar"
    assertEquals(expected, result)
  }

  @Test
  def createImplicit = {
    import Symbol._
    val sym: Symbol = "this.is.a.test#12"
    assertEquals(new Symbol(List("this", "is", "a", "test"), 12), sym)
  }

  @Test
  def compare {
    assertEquals(0, Symbol("a").compare(Symbol("a")))
    assertTrue(Symbol("a").compare(Symbol("b")) < 0)
    assertTrue(Symbol("b").compare(Symbol("a")) > 0)
    assertTrue(Symbol.fromString("a.a").compare(Symbol("a")) > 0)
    assertTrue(Symbol("a").compare(Symbol("a.a")) < 0)
    assertEquals(0, Symbol.fromString("a.a").compare(Symbol.fromString("a.a")))
  }
}
