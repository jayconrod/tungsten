package com.jayconrod.tungsten

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
    val sym = new Symbol(name, 36, Nowhere)
    assertEquals(name, sym.name)
    assertEquals(36, sym.id)
    assertEquals(Nowhere, sym.location)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def emptyName = {
    val sym = new Symbol(Nil, 0, Nowhere)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def emptyNamePart = {
    val sym = new Symbol(List("foo", "", "bar"), 0, Nowhere)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def negativeId = {
    val id = new Symbol(List("foo"), -32, Nowhere)
    ()
  }

  @Test
  def equals = {
    val sym1 = new Symbol("foo", 2, Nowhere)
    val sym2 = new Symbol("foo", 2, Nowhere)
    assertTrue(sym1 == sym2)
  }

  @Test
  def notEquals = {
    val sym1 = new Symbol("foo", 2, Nowhere)
    val sym2 = new Symbol("bar", 3, Nowhere)
    assertFalse(sym1 == sym2)
  }

  @Test
  def hash = {
    val sym1 = new Symbol("foo", 2, Nowhere)
    val sym2 = new Symbol("foo", 2, Nowhere)
    assertEquals(sym1.hashCode, sym2.hashCode)
  }
}
