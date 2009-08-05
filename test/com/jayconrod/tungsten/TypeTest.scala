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
}
