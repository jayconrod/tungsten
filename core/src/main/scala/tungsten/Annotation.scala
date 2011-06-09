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

final case class Annotation(name: Symbol,
                            parameters: List[Symbol],
                            annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def isGlobal = true

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Parameter](module, parameters)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    validateComponentsScope(module, scope, parameters)
  }
}

final case class AnnotationValue(name: Symbol, values: List[Value])
  extends Copying[AnnotationValue]
