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

class ComponentValidationTest
  extends ValidationTest
{
  @Test
  def undefinedParameterType {
    val program = "function unit @main(class @A %x)"
    programContainsError[UndefinedSymbolException](program)
  }

  @Test
  def undefinedTypeParameterBound {
    val program = "function unit @f[type %T <: class @R](type %T %x)"
    programContainsError[UndefinedSymbolException](program)
  }

  @Test
  def duplicateComponent {
    val (instName, blockName) = (Symbol("ret"), Symbol("block"))
    val inst = ReturnInstruction(instName, UnitType, UnitValue)
    val block = Block(blockName, Nil, List(instName, instName))
    val function = Function(Symbol("main"), UnitType, Nil, Nil, List(blockName))
    var module = (new Module).add(inst, block, function)
    containsError[DuplicateComponentException](module.validate)
  }
}
