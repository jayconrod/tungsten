package tungsten.llvm

sealed abstract class Type

final case object VoidType
  extends Type

final case class IntType(width: Int)
  extends Type
{
  if (width <= 0)
    throw new IllegalArgumentException
}

final case object LabelType
  extends Type

final case class PointerType(elementType: Type)
  extends Type

