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
import java.io.File
import Utilities._

class UtilitiesTest {
  @Test
  def testAlign {
    assertEquals(12, align(12, 4))
    assertEquals(16, align(13, 4))
    assertEquals(16, align(15, 4))
    assertEquals(15, align(15, 1))
  }

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
  def fileWithExtension {
    val oldFile = new File("foo.txt")
    val newFile = Utilities.fileWithExtension(oldFile, ".txt", ".jpg")
    assertEquals(new File("foo.jpg").getCanonicalFile, newFile)
  }

  @Test
  def fileWithExtensionMissing {
    val oldFile = new File("foo.asdf")
    val newFile = Utilities.fileWithExtension(oldFile, ".txt", ".jpg")
    assertEquals(new File("foo.asdf.jpg").getCanonicalFile, newFile)
  }

  @Test
  def padMapTest {
    val map = Map((1, 1), (2, 2))
    assertEquals(Map((1, 1), (2, 2), (3, 0)), padMap(map, Set(1, 2, 3), 0))
  }

  @Test
  def parseVersionTest {
    assertEquals(new Version(List(12)), parseVersion("12"))
    assertEquals(new Version(List(0, 1, 2)), parseVersion("0.1.2"))
    assertEquals(new Version(List(34, 45)), parseVersion("34.45"))
  }

  @Test
  def stagedValidation {
    def f(x: Int) = if (x > 0) List(1, 2, 3) else throw new RuntimeException("stage failed")
    stage(f(1), f(-1))
    ()
  }
}
