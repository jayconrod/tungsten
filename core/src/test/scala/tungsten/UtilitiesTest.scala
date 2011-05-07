package tungsten

import org.junit.Test
import org.junit.Assert._
import java.io.File
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
