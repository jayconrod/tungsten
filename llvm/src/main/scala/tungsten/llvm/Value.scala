package tungsten.llvm

sealed abstract class Value {
  def ty: Type
}

final case class IntValue(value: Long, width: Int)
  extends Value
{
  def ty = IntType(width)
}

final case class DefinedValue(name: String, ty: Type)
  extends Value

