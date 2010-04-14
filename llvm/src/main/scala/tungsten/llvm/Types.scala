package tungsten.llvm

sealed abstract class Type

final case object VoidType
  extends Type

final case class IntType(width: Int)
  extends Type

final case object LabelType
  extends Type

final case class PointerType(elementType: Type)
  extends Type

