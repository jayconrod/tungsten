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

import Utilities._

sealed abstract class Value {
  def ty: Type
  def typedToString = ty.toString + " " + toString
}

final case object VoidValue 
  extends Value
{
  def ty = VoidType
  override def toString = "void"
  override def typedToString = toString
}

final case class IntValue(value: Long, width: Int)
  extends Value
{
  def ty = IntType(width)

  override def toString = value.toString
}

final case class FloatValue(value: Double, width: Int)
  extends Value
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException

  def ty = FloatType(width)

  override def toString = "%e".format(value)
}

final case class NullValue(ty: Type) 
  extends Value
{
  override def toString = "null"

  override def typedToString = ty + "* null"
}

final case class ArrayValue(elementType: Type, elements: List[Value])
  extends Value
{
  def ty = ArrayType(elements.size, elementType)

  override def toString = elements.map(_.typedToString).mkString("[", ", ", "]")
}

final case class StructValue(elementTypes: List[Type], elements: List[Value])
  extends Value
{
  def ty = StructType(elements.map(_.ty))

  override def toString = elements.map(_.typedToString).mkString("{", ", ", "}")

  override def typedToString = elementTypes.mkString("{", ", ", "} ") + toString
}

final case class NamedStructValue(name: String, elements: List[Value])
  extends Value
{
  def ty = NamedStructType(name)

  override def toString = elements.map(_.typedToString).mkString("{", ", ", "}")

  override def typedToString = name + " " + toString
}

final case class DefinedValue(name: String, ty: Type)
  extends Value
{
  override def toString = escapeIdentifier(name)
}

final case class BitCastValue(value: Value, ty: Type)
  extends Value
{
  override def toString = "bitcast (%s to %s)".format(value.typedToString, ty)
}
