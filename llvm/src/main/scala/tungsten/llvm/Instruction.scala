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
  override def toString = name + " = alloca " + elementType
}

final case class BitcastInstruction(override name: String,
                                    value: Value,
                                    ty: Type)
  extends Instruction(name)
{
  def operands = List(value)
  override def toString = name + " = bitcast " + value.typedToString + " to " + ty
}

final case class BranchInstruction(label: Value)
  extends Instruction("")
{
  def ty = VoidType
  def operands = List(label)
  override def toString = "br " + label
}

final case class LoadInstruction(override name: String,
                                 address: Value,
                                 alignment: Option[Int])
  extends Instruction(name)
{
  def ty = address.ty.asInstanceOf[PointerType].elementType
  def operands = List(address)
  override def toString = {
    val alignmentStr = alignment.map(", " + _).getOrElse("")
    name + " = load " + address.typedToString  + alignmentStr
  }
}

final case class PhiInstruction(override name: String,
                                ty: Type,
                                bindings: List[(Value, String)])
  extends Instruction(name)
{
  def operands = Nil
  override def toString = {
    val bindingsStr = bindings.map { b => "[" + b._1 + ", " + b._2 + "]" }.mkString(",")
    name + " = phi " + ty + bindingsStr
  }
}

final case class ReturnInstruction(value: Value)
  extends Instruction("")
{
  def ty = VoidType
  def operands = List(value)
  override def toString = "ret " + value.typedToString
}

final case class StoreInstruction(value: Value,
                                  address: Value,
                                  alignment: Option[Int])
  extends Instruction("")
{
  def ty = VoidType
  def operands = List(value, address)
  override def toString = {
    val alignmentStr = alignment.map(", align " + _).getOrElse("")
    "store " + value.typedToString + ", " + address.typedToString + alignmentStr
  }
}
