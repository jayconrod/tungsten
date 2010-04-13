package tungsten.llvm

sealed abstract class Instruction(val name: String) {
  def ty: Type
}

final case class AllocaInstruction(override name: String,
                                   elementType: Type)
  extends Instruction(name)
{
  def ty = PointerType(elementType)
}

final case class BitcastInstruction(override name: String,
                                    value: Value,
                                    ty: Type)
  extends Instruction(name)

final case class BranchInstruction(label: Value)
  extends Instruction("")
{
  def ty = VoidType
}

final case class LoadInstruction(override name: String,
                                 address: Value)
  extends Instruction(name)
{
  def ty = address.ty.asInstanceOf[PointerType].elementType
}

final case class ReturnInstruction(value: Value)
  extends Instruction("")
{
  def ty = VoidType
}

final case class StoreInstruction(value: Value,
                                  address: Value,
                                  alignment: Int)
  extends Instruction("")
{
  def ty = VoidType
}

