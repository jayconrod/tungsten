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
import Utilities._
import org.junit.Test
import org.junit.Assert._

class ModuleTest {
  val global = Global("g", IntType(8), Some(IntValue(2, 8)))
  val definitions = TreeMap(global.name -> global)
  val module = new Module(definitions=definitions)

  def collect[T](s: Set[T], e: T): Set[T] = s + e

  def testMap(expected: Definition, module: Module) {
    val expectedDefinitions = TreeMap(expected.name -> expected)
    val expectedModule = new Module(definitions=expectedDefinitions)
    assertEquals(expectedModule, module)
  }

  @Test
  def mapSymbols {
    def f(symbol: Symbol): Symbol = "h"
    val expected = Global("h", global.ty, global.value)
    testMap(expected, module.mapSymbols(f))
  }

  @Test
  def mapValues {
    def addTwo(value: Value): Value = {
      value match {
        case IntValue(v, w) => IntValue(v + 2, w)
        case _ => value
      }
    }
    val expected = Global("g", IntType(8), Some(IntValue(4, 8)))
    testMap(expected, module.mapValues(addTwo))
  }

  @Test
  def mapTypes {
    def widen(ty: Type): Type = {
      ty match {
        case IntType(w) => IntType(w * 2)
        case _ => ty
      }
    }
    val expected = Global("g", IntType(16), Some(IntValue(2, 8)))
    testMap(expected, module.mapTypes(widen))
  }

  @Test
  def foldSymbols {
    assertEquals(Set(global.name), module.foldSymbols[Set[Symbol]](Set(), collect))
  }

  @Test
  def foldValues {
    assertEquals(Set(global.value.get), module.foldValues[Set[Value]](Set(), collect))
  }

  @Test
  def foldTypes {
    assertEquals(Set(global.ty), module.foldTypes[Set[Type]](Set(), collect))
  }
}
