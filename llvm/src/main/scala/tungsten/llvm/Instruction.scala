package tungsten.llvm

import Utilities._

sealed abstract class Instruction(val name: String) {
  def ty(module: Module): Type
  def operands: List[Value]
  def usedVars: List[String] = operands collect { 
    case DefinedValue(name, ty) if ty != LabelType => name 
  }
}

final case class AllocaInstruction(override name: String,
                                   elementType: Type)
  extends Instruction(name)
{
  def ty(module: Module) = PointerType(elementType)
  def operands = Nil
  override def toString = escapeIdentifier(name) + " = alloca " + elementType
}

final case class BitcastInstruction(override name: String,
                                    value: Value,
                                    ty: Type)
  extends Instruction(name)
{
  def ty(module: Module) = ty
  def operands = List(value)
  override def toString = escapeIdentifier(name) + " = bitcast " + value.typedToString + " to " + ty
}

final case class BranchInstruction(label: Value)
  extends Instruction("")
{
  def ty(module: Module) = VoidType
  def operands = List(label)
  override def toString = "br " + label
}

final case class LoadInstruction(override name: String,
                                 address: Value,
                                 alignment: Option[Int])
  extends Instruction(name)
{
  def ty(module: Module) = address.ty.asInstanceOf[PointerType].elementType
  def operands = List(address)
  override def toString = {
    val alignmentStr = alignment.map(", " + _).getOrElse("")
    escapeIdentifier(name) + " = load " + address.typedToString  + alignmentStr
  }
}

final case class PhiInstruction(override name: String,
                                ty: Type,
                                bindings: List[(Value, String)])
  extends Instruction(name)
{
  def ty(module: Module) = ty
  def operands = Nil
  override def toString = {
    val bindingsStrs = bindings.map { b => 
      "[" + b._1 + ", " + escapeIdentifier(b._2) + "]"
    }
    name + " = phi " + ty + bindingsStrs.mkString(", ")
  }
}

final case class ReturnInstruction(value: Value)
  extends Instruction("")
{
  def ty(module: Module) = VoidType
  def operands = List(value)
  override def toString = "ret " + value.typedToString
}

final case class StoreInstruction(value: Value,
                                  address: Value,
                                  alignment: Option[Int])
  extends Instruction("")
{
  def ty(module: Module) = VoidType
  def operands = List(value, address)
  override def toString = {
    val alignmentStr = alignment.map(", align " + _).getOrElse("")
    "store " + value.typedToString + ", " + address.typedToString + alignmentStr
  }
}
