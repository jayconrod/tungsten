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

object TestUtilities {
  private def _assertEquals(expected: AnyRef, actual: AnyRef) {
    if (expected != actual) {
      throw new RuntimeException("expected <<<%s>>> but was <<<%s>>>".format(expected, actual))
    }
  }        

  def assertSymbolsEqual(expected: Traversable[Symbol], actual: Traversable[Symbol]) {
    _assertEquals(expected.map(nullifySymbol _), actual.map(nullifySymbol _))
  }

  def assertEqualsIgnoreSymbols(expected: Definition, actual: Definition) {
    _assertEquals(expected.mapSymbols(nullifySymbol _),
                  actual.mapSymbols(nullifySymbol _))
  }

  def assertEqualsIgnoreSymbols(expected: Module, actual: Module) {
    expected.definitions.values foreach { expectedDefn =>
      val name = nullifySymbol(expectedDefn.name)
      val matchingDefns = actual.definitions.values.filter { defn =>
        nullifySymbol(defn.name) == name && defn.getClass == expectedDefn.getClass
      }
      matchingDefns match {
        case actualDefn :: Nil => assertEqualsIgnoreSymbols(expectedDefn, actualDefn)
        case Nil => throw new RuntimeException("no matching definitions for symbol: " + expectedDefn.name)
        case _ => ()  // ignore multiple definitions with same name
      }
    }
  }

  def nullifySymbol(sym: Symbol): Symbol = {
    sym.copy(id = 0)
  }
}
