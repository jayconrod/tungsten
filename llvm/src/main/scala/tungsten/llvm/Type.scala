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

final case class FunctionType(returnType: Type, parameterTypes: List[Type])
  extends Type
{
  override def toString = returnType + parameterTypes.mkString(" (", ", ", ")") + "*"
}
