package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class UtilitiesTest {
  @Test 
  def isPowerOf2Test {
    assertTrue(isPowerOf2(0))
    assertTrue(isPowerOf2(1))
    assertTrue(isPowerOf2(2))
    assertTrue(isPowerOf2(4))
    assertTrue(isPowerOf2(8))
    assertFalse(isPowerOf2(7))
  }

  @Test
  def joinStringsTest {
    assertEquals("", joinStrings(",", Nil))
    assertEquals("foo", joinStrings(",", List("foo")))
    assertEquals("foo,bar,baz", joinStrings(",", List("foo", "bar", "baz")))
  }
}
