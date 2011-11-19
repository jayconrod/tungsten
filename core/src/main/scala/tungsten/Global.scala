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

import Utilities._

final case class Global(name: Symbol,
                        ty: Type,
                        value: Option[Value],
                        annotations: List[AnnotationValue] = Nil)
  extends Definition
  with TypedDefinition
{
  override def isGlobal = true

  /** Returns a pointer type to the global type.
   *
   *  This is necessary because defined values which refer to globals are pointers.
   *  Globals could not be modified otherwise since all Tungsten values are immutable.
   */
  def ty(module: Module): Type = PointerType(ty)

  override def validate(module: Module) = {
    def validateValueLiteral = {
      value match {
        case Some(_: DefinedValue) => List(GlobalValueNonLiteralException(name, getLocation))
        case _ => Nil
      }
    }

    stage(super.validate(module),
          validateValueLiteral,
          value.toList.flatMap { v => checkType(v.ty, ty, getLocation) })
  }

  def makeValue: DefinedValue = {
    DefinedValue(name, PointerType(ty))
  }
}
