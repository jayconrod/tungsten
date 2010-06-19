package tungsten.llvm

import Utilities._

sealed abstract class Instruction {
  def name: String = ""
  def opname: String
  def ty(module: Module): Type
  def operands: List[Value]
  def usedVars: List[String] = operands collect { 
    case DefinedValue(name, ty) if ty != LabelType => name 
  }
  protected final def nameString: String = {
    if (name.isEmpty)
      opname
    else
      escapeIdentifier(name) + " = " + opname
  }
}

sealed abstract class BinaryOperatorInstruction extends Instruction {
  def ty: Type
  def ty(module: Module): Type = ty
  def left: Value
  def right: Value
  def operands = List(left, right)
  override def toString = "%s %s %s, %s".format(nameString, ty, left, right)
}

sealed abstract class ConversionInstruction extends Instruction {
  def ty: Type
  def ty(module: Module) = ty
  def value: Value
  def operands = List(value)
  override def toString = "%s %s to %s".format(nameString, value.typedToString, ty)
}

final case class Comparison(name: String) {
  override def toString = name
}
object Comparison {
  /* Float comparisons
   * Ordered means neither operand is NaN. Operators that begin with O are ordered. 
   * For instance, OGT means ordered AND greater than. Operators that begin with U
   * are unordered. For instance, UGT means unordered OR greater than. 
   */
  val FALSE = Comparison("false")   // always false
  val OEQ = Comparison("oeq")
  val OGT = Comparison("ogt")
  val OGE = Comparison("oge")
  val OLT = Comparison("olt")
  val OLE = Comparison("ole")
  val ONE = Comparison("one")
  val ORD = Comparison("ord")       // ordered
  val UEQ = Comparison("ueq")
  val UGT = Comparison("ugt")
  val UGE = Comparison("uge")
  val ULT = Comparison("ult")
  val ULE = Comparison("ule")
  val UNE = Comparison("une")
  val UNO = Comparison("uno")       // unordered
  val TRUE = Comparison("true")     // always true

  /* Integer comparisons
   * These work for both integer values and pointers. S means signed. Note that 
   * UGT, UGE, ULT, and ULE are considered unsigned comparisons when used in an 
   * integer context.
   */
  val EQ = Comparison("eq")
  val NE = Comparison("ne")
  val SGT = Comparison("sgt")
  val SGE = Comparison("sge")
  val SLT = Comparison("slt")
  val SLE = Comparison("sle")
}  

final case class AddInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "add"
}
  
final case class AllocaInstruction(override name: String,
                                   elementType: Type)
  extends Instruction
{
  def opname = "alloca"
  def ty(module: Module) = PointerType(elementType)
  def operands = Nil
  override def toString = nameString + " " + elementType
}

final case class AllocaArrayInstruction(override name: String,
                                        elementType: Type,
                                        count: Value)
  extends Instruction
{
  def opname = "alloca"
  def ty(module: Module) = PointerType(elementType)
  def operands = List(count)
  override def toString = nameString + " " + elementType + ", " + count
}

final case class AndInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "and"
}

final case class ArithmeticShiftRightInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "asr"
}

final case class BitCastInstruction(override name: String,
                                    value: Value,
                                    ty: Type)
  extends ConversionInstruction
{
  def opname = "bitcast"
}

final case class BranchInstruction(label: Value)
  extends Instruction
{
  def opname = "br"
  def ty(module: Module) = VoidType
  def operands = List(label)
  override def toString = nameString + " " + label.typedToString
}

final case class CallInstruction(override name: String,
                                 isTailCall: Boolean,
                                 convention: Option[String],
                                 returnAttributes: List[Attribute],
                                 ty: Type,
                                 targetType: Option[Type],
                                 target: Value,
                                 arguments: List[Value],
                                 functionAttributes: List[Attribute])
  extends Instruction
{
  def opname = if (isTailCall) "tail call" else "call"
  def ty(module: Module) = ty
  def operands = target :: arguments
  override def toString = {
    "%s %s%s%s %s%s(%s)%s".
      format(if (ty == VoidType) opname else nameString,
             convention.map(_ + " ").getOrElse(""),
             if (!returnAttributes.isEmpty) returnAttributes.mkString(" ") + " " else "",
             ty,
             targetType.map(_.toString + " ").getOrElse(""),
             target,
             arguments.map(_.typedToString).mkString(", "),
             if (!functionAttributes.isEmpty) " " + functionAttributes.mkString(" ") else "")
  }
}             

final case class ConditionalBranchInstruction(condition: Value, 
                                              trueTarget: Value, 
                                              falseTarget: Value)
  extends Instruction
{
  def opname = "br"
  def ty(module: Module) = VoidType
  def operands = List(condition, trueTarget, falseTarget)
  override def toString = {
    "br %s, %s, %s".
      format(condition.typedToString, trueTarget.typedToString, falseTarget.typedToString)
  }
}

final case class FloatAddInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fadd"
}

final case class FloatCompareInstruction(override name: String,
                                         comparison: Comparison,
                                         ty: Type,
                                         left: Value,
                                         right: Value)
  extends Instruction
{
  def opname = "fcmp"
  def ty(module: Module) = IntType(1)
  def operands = List(left, right)
  override def toString = {
    "%s %s %s %s, %s".format(nameString, comparison, ty, left, right)
  }
}

final case class FloatDivideInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fdiv"
}

final case class FloatExtendInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fpext"
}

final case class FloatMultiplyInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fmul"
}

final case class FloatRemainderInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "frem"
}

final case class FloatSubtractInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fsub"
}

final case class FloatToSignedIntegerInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fptosi"
}

final case class FloatToUnsignedIntegerInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fptoui"
}

final case class FloatTruncateInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fptrunc"
}

final case class GetElementPointerInstruction(override name: String,
                                              base: Value,
                                              indices: List[Value])
  extends Instruction
{
  def opname = "getelementptr"

  def ty(module: Module) = {
    def getStructFieldType(structType: Type, index: Value): Type = {
      val ix = index match {
        case IntValue(i, 32) => i
        case _ => throw new LlvmConversionException("invalid index value for struct: " + index)
      }
      val fieldTypes = structType match {
        case StructType(fts) => fts
        case NamedStructType(name) => {
          module.definitions.get(name) match {
            case Some(Struct(_, fts)) => fts
            case _ => throw new LlvmConversionException("struct name %s does not correspond to a defined struct type".format(name))
          }
        }
        case _ => throw new RuntimeException("struct type expected")
      }
      if (0 <= ix && ix < fieldTypes.size)
        fieldTypes(ix.toInt)
      else
        throw new LlvmConversionException("struct type %s does not have a field at index %d".format(structType, ix))
    }

    def getElementType(baseType: Type, indices: List[Value]): Type = {
      indices match {
        case Nil => baseType
        case i :: is => {
          val elementType = baseType match {
            case _: StructType | _: NamedStructType => getStructFieldType(baseType, i)
            case ArrayType(_, elementType) => elementType
            case _ => throw new LlvmConversionException("type %s is not indexable".format(baseType))
          }
          getElementType(elementType, is)
        }
      }
    }

    val baseType = base.ty match {
      case PointerType(t) => t
      case _ => throw new LlvmConversionException("base %s is not a pointer".format(base))
    }
    val elementType = getElementType(baseType, indices.tail)
    PointerType(elementType)
  }

  def operands = base :: indices

  override def toString = {
    "%s %s, %s".format(nameString, 
                       base.typedToString, 
                       indices.map(_.typedToString).mkString(", "))
  }
}
    
final case class IntegerCompareInstruction(override name: String,
                                           comparison: Comparison,
                                           ty: Type,
                                           left: Value,
                                           right: Value)
  extends Instruction
{
  def opname = "icmp"
  def ty(module: Module) = IntType(1)
  def operands = List(left, right)
  override def toString = {
    "%s %s %s %s, %s".format(nameString, comparison, ty, left, right)
  }
}

final case class IntegerSignExtendInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "sext"
}

final case class IntegerTruncateInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "trunc"
}

final case class IntegerZeroExtendInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "zext"
}

final case class LoadInstruction(override name: String,
                                 address: Value,
                                 alignment: Option[Int])
  extends Instruction
{
  def opname = "load"
  def ty(module: Module) = address.ty.asInstanceOf[PointerType].elementType
  def operands = List(address)
  override def toString = {
    val alignmentStr = alignment.map(", align " + _).getOrElse("")
    nameString + " " + address.typedToString  + alignmentStr
  }
}

final case class LogicalShiftRightInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "lsr"
}

final case class MultiplyInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "mul"
}

final case class OrInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "or"
}

final case class PhiInstruction(override name: String,
                                ty: Type,
                                bindings: List[(Value, String)])
  extends Instruction
{
  def opname = "phi"
  def ty(module: Module) = ty
  def operands = Nil
  override def toString = {
    val bindingsStrs = bindings.map { b => 
      "[" + b._1 + ", " + escapeIdentifier(b._2) + "]"
    }
    "%s %s %s".format(nameString, ty, bindingsStrs.mkString(", "))
  }
}

final case class ReturnInstruction(value: Value)
  extends Instruction
{
  def opname = "ret"
  def ty(module: Module) = VoidType
  def operands = List(value)
  override def toString = "ret " + value.typedToString
}

final case class ShiftLeftInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "shl"
}

final case class SignedDivideInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "sdiv"
}

final case class SignedIntegerToFloatInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "sitofp"
}

final case class SignedRemainderInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "srem"
}

final case class StoreInstruction(value: Value,
                                  address: Value,
                                  alignment: Option[Int])
  extends Instruction
{
  def opname = "store"
  def ty(module: Module) = VoidType
  def operands = List(value, address)
  override def toString = {
    val alignmentStr = alignment.map(", align " + _).getOrElse("")
    "store " + value.typedToString + ", " + address.typedToString + alignmentStr
  }
}

final case class SubtractInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "sub"
}

final case object UnreachableInstruction
  extends Instruction
{
  def opname = "unreachable"
  def ty(module: Module) = VoidType
  def operands = Nil
  override def toString = opname
}

final case class UnsignedDivideInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "udiv"
}

final case class UnsignedIntegerToFloatInstruction(override name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "uitofp"
}

final case class UnsignedRemainderInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "urem"
}

final case class XorInstruction(override name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "xor"
}
