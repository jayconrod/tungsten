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
