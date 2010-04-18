package tungsten.llvm

sealed abstract class Instruction(val name: String) {
  def ty: Type
  def operands: List[Value]
  def usedVars: List[String] = operands collect { 
    case DefinedValue(name, ty) if ty != LabelType => name 
  }
}

final case class AllocaInstruction(override name: String,
                                   elementType: Type)
  extends Instruction(name)
{
  def ty = PointerType(elementType)
  def operands = Nil
}

final case class BitcastInstruction(override name: String,
                                    value: Value,
                                    ty: Type)
  extends Instruction(name)
{
  def operands = List(value)
}

final case class BranchInstruction(label: Value)
  extends Instruction("")
{
  def ty = VoidType
  def operands = List(label)
}

final case class LoadInstruction(override name: String,
                                 address: Value)
  extends Instruction(name)
{
  def ty = address.ty.asInstanceOf[PointerType].elementType
  def operands = List(address)
}

final case class ReturnInstruction(value: Value)
  extends Instruction("")
{
  def ty = VoidType
  def operands = List(value)
}

final case class StoreInstruction(value: Value,
                                  address: Value,
                                  alignment: Int)
  extends Instruction("")
{
  def ty = VoidType
  def operands = List(value, address)
}

