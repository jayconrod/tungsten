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
  override def toString = ty.toString + " " + null
}

final case class ArrayValue(elementType: Type, elements: List[Value])
  extends Value
{
  def ty = ArrayType(elements.size, elementType)

  override def toString = elements.mkString("[", ", ", "]")
}

final case class StructValue(elements: List[Value])
  extends Value
{
  def ty = throw new UnsupportedOperationException // TODO

  override def toString = elements.mkString("{", ", ", "}")
}

final case class DefinedValue(name: String, ty: Type)
  extends Value
{
  override def toString = escapeIdentifier(name)
}

