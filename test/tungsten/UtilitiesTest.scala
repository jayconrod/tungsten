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
  def fileWithExtension = {
    val oldFile = new File("foo.txt")
    val newFile = Utilities.fileWithExtension(oldFile, ".txt", ".jpg")
    assertEquals(new File("foo.jpg").getCanonicalFile, newFile)
  }

  @Test
  def fileWithExtensionMissing = {
    val oldFile = new File("foo.asdf")
    val newFile = Utilities.fileWithExtension(oldFile, ".txt", ".jpg")
    assertEquals(new File("foo.asdf.jpg").getCanonicalFile, newFile)
  }
}
