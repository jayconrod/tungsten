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

  @Test
  def floatEquals = {
    val f1 = FloatType(32)
    val f2 = FloatType(32)
    assertTrue(f1 == f2)
  }

  @Test
  def floatHash = {
    val f1 = FloatType(32)
    val f2 = FloatType(32)
    assertEquals(f1.hashCode, f2.hashCode)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def badFloat = {
    val f = FloatType(16)
    ()
  }

  @Test
  def arrayEquals = {
    val et = IntType(32)
    val a1 = ArrayType(et)
    val a2 = ArrayType(et)
    assertTrue(a1 == a2)
  }

  @Test
  def arrayHash = {
    val et = IntType(32)
    val a1 = ArrayType(et)
    val a2 = ArrayType(et)
    assertEquals(a1.hashCode, a2.hashCode)
  }

  @Test
  def functionEquals = {
    val f1 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    val f2 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    assertTrue(f1 == f2)
  }

  @Test
  def functionHash = {
    val f1 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    val f2 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    assertEquals(f1.hashCode, f2.hashCode)
  }
}
