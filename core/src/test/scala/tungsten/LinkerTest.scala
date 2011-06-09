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

import scala.collection.immutable.TreeMap
import org.junit.Test
import org.junit.Assert._
import Utilities._
import Linker._

class LinkerTest {
  @Test
  def parseLibraryTest {
    def test(libStr: String, 
             expectedName: String, 
             expectedMinVersion: String, 
             expectedMaxVersion: String) 
    {
      libStr match {
        case LibraryArgument(name, minVersion, maxVersion) => {
          assertEquals(expectedName, name)
          assertEquals(expectedMinVersion, minVersion)
          assertEquals(expectedMaxVersion, maxVersion)
        }
        case _ => fail("library string did not match: " + libStr)
      }
    }
    test("-lfoo", "foo", null, null)
    test("-lfoo.bar", "foo.bar", null, null)
    test("-lfoo:0.1", "foo", "0.1", "")
    test("-lfoo:0.1-", "foo", "0.1", "")
    test("-lfoo:-0.1", "foo", "", "0.1")
    test("-lfoo:0.1-2.3", "foo", "0.1", "2.3")
  }

  @Test
  def isStrongTest {
    assertFalse(isStrong(Function("f", UnitType, Nil, Nil, Nil)))
    assertTrue(isStrong(Function("f", UnitType, Nil, Nil, List("p"))))
    assertFalse(isStrong(Global("g", UnitType, None)))
    assertTrue(isStrong(Global("g", UnitType, Some(UnitValue))))
    assertFalse(isStrong(Parameter("p", UnitType)))
    assertTrue(isStrong(Struct("s", Nil)))
  }

  @Test
  def mergedDependencies {
    val dependencies @ List(a, b, c, d) = List("a", "b", "c", "d").map { 
      new ModuleDependency(_, Version.MIN, Version.MAX)
    }
    val definitions = new TreeMap[Symbol, Definition]()
    val m1 = new Module(dependencies = List(b, c), is64Bit = true, definitions = definitions)
    val m2 = new Module(dependencies = List(d, c), is64Bit = true, definitions = definitions)
    val linked = Linker.linkModules(List(m1, m2), "default", ModuleType.INTERMEDIATE,
                                    Version.MIN, None, List(a), Nil)
    assertEquals(dependencies, linked.dependencies)
  }
}
