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

sealed abstract class Type

final case object VoidType
  extends Type
{
  override def toString = "void"
}

final case class IntType(width: Int)
  extends Type
{
  if (width <= 0)
    throw new IllegalArgumentException

  override def toString = "i" + width
}

final case class FloatType(width: Int)
  extends Type
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException

  override def toString = if (width == 32) "float" else "double"
}

final case object LabelType
  extends Type
{
  override def toString = "label"
}

final case class PointerType(elementType: Type)
  extends Type
{
  override def toString = elementType + "*"
}

final case class ArrayType(size: Long, elementType: Type)
  extends Type
{
  override def toString = "[%d x %s]".format(size, elementType)
}

final case class StructType(fieldTypes: List[Type])
  extends Type
{
  override def toString = fieldTypes.mkString("{", ", ", "}")
}

final case class NamedStructType(name: String)
  extends Type
{
  override def toString = name
}

final case class FunctionType(returnType: Type, 
                              parameterTypes: List[Type], 
                              isVariadic: Boolean = false)
  extends Type
{
  override def toString = {
    "%s (%s%s)".format(returnType,
                       parameterTypes.mkString(", "),
                       if (isVariadic) ", ..." else "")
  }
}
