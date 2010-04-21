package tungsten.llvm

import Utilities._

sealed abstract class Value {
  def ty: Type
  def typedToString = ty.toString + " " + toString
}

final case class IntValue(value: Long, width: Int)
  extends Value
{
  def ty = IntType(width)

  override def toString = value.toString
}

final case class DefinedValue(name: String, ty: Type)
  extends Value
{
  override def toString = escapeIdentifier(name)
}

