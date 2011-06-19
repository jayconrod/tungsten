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
import Utilities._

class DefinitionTest {
  @Test
  def extractFileLocation {
    val defn = Global("foo", UnitType, None, 
                      List(AnnotationValue("tungsten.Location",
                                           List(StringValue("foo.w"),
                                                IntValue(1, 32),
                                                IntValue(2, 32),
                                                IntValue(3, 32),
                                                IntValue(4, 32)))))
    assertEquals(FileLocation("foo.w", 1, 2, 3, 4), defn.getLocation)
  }

  @Test
  def extractSymbolLocation {
    val sym = Symbol("foo")
    val defn = Global("foo", UnitType, None)
    assertEquals(SymbolLocation(sym), defn.getLocation)
  }
}
