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

final case object FloatType
  extends Type
{
  override def toString = "float"
}

final case object DoubleType
  extends Type
{
  override def toString = "double"
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

final case class StructType(name: String)
  extends Type
{
  override def toString = "struct " + name
}
