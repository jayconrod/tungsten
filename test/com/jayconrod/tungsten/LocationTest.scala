package com.jayconrod.tungsten

import org.junit.Test
import org.junit.Assert._

class LocationTest {
  @Test
  def create = {
    val loc = new Location("file.w", 1, 2, 3, 4)
    assertEquals("file.w", loc.filename)
    assertEquals(1, loc.beginLine)
    assertEquals(2, loc.beginColumn)
    assertEquals(3, loc.endLine)
    assertEquals(4, loc.endColumn)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def nameEmpty = {
    val loc = new Location("", 1, 2, 3, 4)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def createBeginLineZero: Unit = {
    val loc = new Location("file.w", 0, 1, 1, 1)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def createBeginColumnZero = {
    val loc = new Location("file.w", 1, 0, 1, 1)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def createEndColumnZero = {
    val loc = new Location("file.w", 1, 1, 2, 0)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def createBeginLineAfterEndLine = {
    val loc = new Location("file.w", 2, 1, 1, 1)
    ()
  }

  @Test(expected=classOf[IllegalArgumentException])
  def createBeginColumnAfterEndColumn = {
    val loc = new Location("file.w", 1, 2, 1, 1)
    ()
  }

  @Test
  def equals = {
    val l1 = new Location("file.w", 1, 2, 3, 4)
    val l2 = new Location("file.w", 1, 2, 3, 4)
    assertEquals(true, l1 == l2)
  }

  @Test
  def notEquals = {
    val l1 = new Location("file.w", 1, 2, 3, 4)
    val l2 = new Location("other.w", 5, 6, 7, 8)
    assertEquals(false, l1 == l2)
  }

  @Test
  def hash = {
    val l1 = new Location("file.w", 1, 2, 3, 4)
    val l2 = new Location("file.w", 1, 2, 3, 4)
    assertEquals(l1.hashCode, l2.hashCode)
  }
}

