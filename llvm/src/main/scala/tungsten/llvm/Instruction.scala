/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

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

sealed abstract class ElementInstruction extends Instruction {
  def getElementType(ty: Type, indices: List[Value], module: Module): Type = {
    def getConstantIndexValue(index: Value, bound: Int): Int = {
      index match {
        case IntValue(ix, 32) if 0 <= ix && ix < bound => ix.toInt
        case IntValue(_, 32) => throw new LlvmConversionException("index is out of bounds: " + index)
        case _ => throw new LlvmConversionException("value cannot be used to index a struct type: " + index)
      }
    }

    indices match {
      case Nil => ty
      case i :: is => {
        ty match {
          case ArrayType(_, elementType) => getElementType(elementType, is, module)
          case StructType(fieldTypes) => {
            val index = getConstantIndexValue(i, fieldTypes.size)
            getElementType(fieldTypes(index.toInt), is, module)
          }
          case NamedStructType(structName) => {
            val struct = module.definitions.get(structName) match {
              case Some(s: Struct) => s
              case None => throw new LlvmConversionException("undefined struct: " + structName)
            }
            val index = getConstantIndexValue(i, struct.fieldTypes.size)
            getElementType(struct.fieldTypes(index.toInt), is, module)
          }
          case _ => throw new LlvmConversionException("invalid aggregate type: " + ty)
        }
      }
    }
  }
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

final case class AddInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "add"
}
  
final case class AllocaInstruction(override val name: String,
                                   elementType: Type)
  extends Instruction
{
  def opname = "alloca"
  def ty(module: Module) = PointerType(elementType)
  def operands = Nil
  override def toString = nameString + " " + elementType
}

final case class AllocaArrayInstruction(override val name: String,
                                        elementType: Type,
                                        count: Value)
  extends Instruction
{
  def opname = "alloca"
  def ty(module: Module) = PointerType(elementType)
  def operands = List(count)
  override def toString = nameString + " " + elementType + ", i32 " + count
}

final case class AndInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "and"
}

final case class ArithmeticShiftRightInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "asr"
}

final case class BitCastInstruction(override val name: String,
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

final case class CallInstruction(override val name: String,
                                 isTailCall: Boolean,
                                 convention: Option[String],
                                 returnAttributes: Set[ParameterAttribute],
                                 ty: Type,
                                 targetType: Option[Type],
                                 target: Value,
                                 arguments: List[Value],
                                 functionAttributes: Set[FunctionAttribute])
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

final case class ExtractValueInstruction(override val name: String,
                                         value: Value,
                                         indices: List[Value])
  extends ElementInstruction
{
  def opname = "extractvalue"
  def ty(module: Module) = getElementType(value.ty, indices, module)
  def operands = value :: indices
  override def toString = {
    "%s %s, %s".format(nameString, 
                       value.typedToString, 
                       indices.mkString(", "))
  }
}

final case class FloatAddInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fadd"
}

final case class FloatCompareInstruction(override val name: String,
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

final case class FloatDivideInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fdiv"
}

final case class FloatExtendInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fpext"
}

final case class FloatMultiplyInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fmul"
}

final case class FloatRemainderInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "frem"
}

final case class FloatSubtractInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "fsub"
}

final case class FloatToSignedIntegerInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fptosi"
}

final case class FloatToUnsignedIntegerInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fptoui"
}

final case class FloatTruncateInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "fptrunc"
}

final case class GetElementPointerInstruction(override val name: String,
                                              base: Value,
                                              indices: List[Value])
  extends ElementInstruction
{
  def opname = "getelementptr"

  def ty(module: Module) = {
    (base.ty, indices) match {
      case (PointerType(baseType), i :: is) => {
        val elementType = getElementType(baseType, is, module)
        PointerType(elementType)
      }
      case (PointerType(_), Nil) => throw new LlvmConversionException("at least one index must be given for getelementptr")
      case _ => throw new LlvmConversionException("first argument to getelementptr must be pointer")
    }
  }

  def operands = base :: indices

  override def toString = {
    "%s %s, %s".format(nameString, 
                       base.typedToString, 
                       indices.map(_.typedToString).mkString(", "))
  }
}
    
final case class InsertValueInstruction(override val name: String,
                                        base: Value,
                                        value: Value,
                                        indices: List[Value])
  extends ElementInstruction
{
  def opname = "insertvalue"
  def ty(module: Module) = base.ty
  def operands = base :: value :: indices
  override def toString = {
    "%s %s, %s, %s".format(nameString, 
                           base.typedToString, 
                           value.typedToString, 
                           indices.mkString(", "))
  }
}

final case class IntegerCompareInstruction(override val name: String,
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

final case class IntegerSignExtendInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "sext"
}

final case class IntegerToPointerInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "inttoptr"
}

final case class IntegerTruncateInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "trunc"
}

final case class IntegerZeroExtendInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "zext"
}

final case class LoadInstruction(override val name: String,
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

final case class LogicalShiftRightInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "lsr"
}

final case class MultiplyInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "mul"
}

final case class OrInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "or"
}

final case class PhiInstruction(override val name: String,
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

final case class PointerToIntegerInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "ptrtoint"
}

final case class ReturnInstruction(value: Value)
  extends Instruction
{
  def opname = "ret"
  def ty(module: Module) = VoidType
  def operands = List(value)
  override def toString = "ret " + value.typedToString
}

final case class ShiftLeftInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "shl"
}

final case class SignedDivideInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "sdiv"
}

final case class SignedIntegerToFloatInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "sitofp"
}

final case class SignedRemainderInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "srem"
}

final case class SignExtendInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "sext"
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

final case class SubtractInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "sub"
}

final case class TruncateInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "trunc"
}

final case object UnreachableInstruction
  extends Instruction
{
  def opname = "unreachable"
  def ty(module: Module) = VoidType
  def operands = Nil
  override def toString = opname
}

final case class UnsignedDivideInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "udiv"
}

final case class UnsignedIntegerToFloatInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "uitofp"
}

final case class UnsignedRemainderInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "urem"
}

final case class XorInstruction(override val name: String, ty: Type, left: Value, right: Value)
  extends BinaryOperatorInstruction
{
  def opname = "xor"
}

final case class ZeroExtendInstruction(override val name: String, value: Value, ty: Type)
  extends ConversionInstruction
{
  def opname = "zext"
}
