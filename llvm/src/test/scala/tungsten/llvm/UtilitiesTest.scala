package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import Utilities._

class UtilitiesTest {
  @Test
  def hexDigitTest {
    assertEquals('0', hexDigit(0))
    assertEquals('9', hexDigit(9))
    assertEquals('A', hexDigit(10))
    assertEquals('F', hexDigit(15))
  }

  @Test
  def escapeIdentifierTest {
    assertEquals("%x", escapeIdentifier("%x"))
    assertEquals("%_", escapeIdentifier("%_"))
    assertEquals("%99", escapeIdentifier("%99"))
    assertEquals("%\"9a\"", escapeIdentifier("%9a"))
    assertEquals("%\"multi word\"", escapeIdentifier("%multi word"))
    assertEquals("@global", escapeIdentifier("@global"))
    assertEquals("label", escapeIdentifier("label"))
  }

  @Test
  def escapeStringTest {
    assertEquals("\"basic\"", escapeString("basic"))
    assertEquals("\"multi\\0Aline\"", escapeString("multi\nline"))
    assertEquals("\"\\22quoted\\22\"", escapeString("\"quoted\""))
  }
}
