package com.jayconrod.tungsten

import org.junit.Test
import org.junit.Assert._

class TypeTest {
  @Test
  def unitEquals = {
    val u1 = UnitType
    val u2 = UnitType
    assertTrue(u1 == u2)
  }

  @Test
  def intEquals = {
    val i1 = IntType(32)
    val i2 = IntType(32)
    assertTrue(i1 == i2)
  }

  @Test
  def intHash = {
    val i1 = IntType(32)
    val i2 = IntType(32)
    assertEquals(i1.hashCode, i2.hashCode)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def badInt = {
    val i = IntType(33)
    ()
  }
}
