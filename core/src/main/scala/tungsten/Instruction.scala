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

package tungsten

import Utilities._

sealed abstract class Instruction
  extends Definition
{
  def ty: Type

  def isTerminating = false

  def successors: Set[Symbol] = Set()

  def operands: List[Value]

  /** Collects symbols used by operands. This does not count symbols used inside types, only
   *  names of instructions, parameters, and globals referenced.
   */
  def operandSymbols: List[Symbol] = {
    def collectSymbols(ops: List[Value], syms: List[Symbol]): List[Symbol] = {
      ops match {
        case DefinedValue(value, _) :: rest => collectSymbols(rest, value :: syms)
        case o :: rest => collectSymbols(rest, syms)
        case Nil => syms
      }
    }
    collectSymbols(operands, Nil).reverse
  }

  def usedSymbols = operandSymbols

  def liveOutBindings: Map[Symbol, List[Value]] = Map()

  final def makeValue: DefinedValue = DefinedValue(name, ty)
}

abstract class ExtendedInstruction extends Instruction

trait CallInstruction extends Instruction {
  protected def validateCall(module: Module,
                             targetName: Symbol,
                             targetType: FunctionType,
                             typeArguments: List[Type],
                             arguments: List[Value],
                             expectedReturnType: Type): List[CompileException] = 
  {
    def validateTypeArguments = {
      val typeArgumentCountErrors = if (typeArguments.size != targetType.typeParameters.size) {
        List(TypeArgumentCountException(targetName, 
                                        typeArguments.size,
                                        targetType.typeParameters.size,
                                        getLocation))
      } else
        Nil

      val typeArgumentBoundsErrors = (typeArguments zip targetType.typeParameters) flatMap { p =>
        val (typeArgument, typeParameterName) = p
        val typeParameter = module.getTypeParameter(typeParameterName)
        if (typeParameter.isArgumentInBounds(typeArgument, module))
          Nil
        else
          List(TypeArgumentBoundsException(typeArgument, typeParameter, getLocation))
      }

      typeArgumentCountErrors ++ typeArgumentBoundsErrors
    }

    def validateEverythingElse = {
      val argCount = arguments.size
      val paramCount = targetType.parameterTypes.size
      val isVariadic = targetType.isVariadic
      val argumentCountErrors = if ((isVariadic && argCount < paramCount - 1) ||
                                    (!isVariadic && argCount != paramCount))
      {
        List(FunctionArgumentCountException(targetName,
                                            argCount,
                                            paramCount - 1,
                                            getLocation))
      } else
        Nil

      val substitutedTargetType = targetType.applyTypeArguments(typeArguments)
      val parameterTypes = if (isVariadic)
        substitutedTargetType.parameterTypes.take(paramCount - 1)
      else
        substitutedTargetType.parameterTypes
      val argumentTypes = arguments.map(_.ty)
      val argumentTypeErrors = (argumentTypes zip parameterTypes) flatMap { p =>
        val (argumentType, parameterType) = p
        checkType(argumentType, parameterType, getLocation)
      }

      val returnTypeErrors = checkType(substitutedTargetType.returnType,
                                       expectedReturnType,
                                       getLocation)

      argumentCountErrors ++ argumentTypeErrors ++ returnTypeErrors
    }

    stage(validateTypeArguments,
          validateEverythingElse)
  }
}   

/** A trait for instructions which deal with values within aggregate values. */
trait ElementInstruction extends Instruction {
  /** Returns a list of indices used for calculating the offset past the base. */
  def indices: List[Value]

  /** Determines the type of the element being referenced by this instruction.
   *  @param baseType the type of the base value for this instruction. If the instruction
   *    deals with PointerType, this is the base type of the pointer. If the instruction
   *    deals directly with aggregates, this is the type of the aggregate. If the instruction
   *    deals with ClassType, this is the type of the field the first index refers to (the
   *    first index should be omitted)
   *  @param indices a list of index values. These have 32- or 64-bit integer type depending
   *    on whether the module is 64-bit. Literal integers (IntValue) are needed to index
   *    StructType, but any integer can index ArrayType.
   */
  def getElementType(module: Module, baseType: Type, indices: List[Value]): Type = {
    indices match {
      case Nil => baseType
      case i :: is => {
        baseType match {
          case ArrayType(_, elementType) => getElementType(module, elementType, is)
          case StructType(structName) => {
            val struct = module.getStruct(structName)
            val wordSize = IntType.wordSize(module)
            i match {
              case IntValue(ix, wordSize) if 0 <= ix && ix < struct.fields.size => {
                val field = module.getField(struct.fields(ix.toInt))
                getElementType(module, field.ty, is)
              }
              case _ => throw new RuntimeException("non-integer index found; did you call validateIndices first?")
            }
          }
          case _ => throw new RuntimeException("base type is neither array nor struct; did you call validateIndices first?")
        }
      }
    }
  }       

  /** Checks that the list of indices for this instruction is valid. If this returns Nil, then
   *  getElementType may be called with the list of indices. In order to be valid, all indices
   *  must have 32- or 64-bit integer types depending on the module. Each index must correspond
   *  to an aggregate type (struct or array). Indices for struct types must be constant and must
   *  be at least 0 and less than the number of fields in that struct type.
   *  @param baseType the type of the base value for this instruction. If the instruction
   *    deals with pointers, this is the base type of the pointer. If the instruction
   *    deals directly with aggregates, this is the type of the aggregate.
   *  @param indices the list of index values to check
   */
  def validateIndices(module: Module,
                      baseType: Type, 
                      indices: List[Value]): List[CompileException] =
  {
    val wordType = IntType.wordType(module)
    def check(baseType: Type, 
              indices: List[Value],
              errors: List[CompileException]): List[CompileException] =
    {
      indices match {
        case Nil => errors
        case i :: is => {
          val indexType = i.ty
          val newErrors = if (indexType == wordType)
            errors
          else
            TypeMismatchException(indexType.toString, wordType.toString, getLocation) :: errors

          baseType match {
            case ArrayType(_, elementType) => 
              check(elementType, is, newErrors)
            case StructType(structName) => {
              val struct = module.getStruct(structName)
              val numFields = struct.fields.size
              val wordSize = IntType.wordSize(module)
              i match {
                case IntValue(ix, wordSize) if 0 <= ix && ix < numFields => {
                  val field = module.getField(struct.fields(ix.toInt))
                  check(field.ty, is, newErrors)
                }
                case _ => {
                  val error = InvalidIndexException(i.toString, baseType.toString, getLocation)
                  error :: newErrors
                }
              }
            }
            case _ => {
              val error = InvalidIndexException(i.toString, baseType.toString, getLocation)
              error :: newErrors
            }
          }
        }
      }
    }
    check(baseType, indices, Nil)
  }
}

/* A trait for element instructions where the base is a pointer to an aggregate value */
trait PointerElementInstruction
  extends ElementInstruction
{
  /** Return the base pointer this instruction operates on */
  def base: Value

  /** Like getElementType, but intended for instructions that deal with pointers or class
   *  references. If pointerType is a true PointerType, the first index is treated like
   *  an array index; if that is the only index, you will get back the same type. If
   *  pointerType is a ClassType, this does not apply since objects cannot be allocated
   *  contiguously. The remaining indices are treated normally.
   *  @param pointerType the type of the base pointer value. This must be an addressable pointer.
   *    Call validatePointerIndices first if you aren't sure.
   *  @param indices the indices to the element. These should be validated first by 
   *    validatePointerIndices.
   *  @return a pointer type to the element being referenced by the indices
   */
  def getPointerType(module: Module, pointerType: Type, indices: List[Value]): PointerType = 
  {
    indices match {
      case i :: is => {
        pointerType match {
          case PointerType(baseType, _) => {
            val elementType = getElementType(module, baseType, is)
            PointerType(elementType)
          }
          case ClassType(className, typeArguments, _) => {
            val clas = module.getClass(className)
            val wordSize = IntType.wordSize(module)
            i match {
              case IntValue(ix, wordSize) if 0 <= ix && ix < clas.fields.size => {
                val field = module.getField(clas.fields(ix.toInt))
                val fieldType = field.ty.substitute(clas.typeParameters, typeArguments)
                val elementType = getElementType(module, fieldType, is)
                PointerType(elementType)
              }
              case _ => throw new RuntimeException("non-constant index; did you call validatePointerIndices?")
            }
          }
          case _ => throw new RuntimeException("non-addressable pointer type; did you call validatePointerIndices?")
        }
      }
      case Nil => throw new RuntimeException("no indices; did you call validatePointerIndices?")
    }
  }

  /** Like validateIndicies, but intended for instructions that deal with pointers.
   *  If pointerType is a PointerType, the first index in these instructions is treated like an
   *  array index. If pointerType is a ClassType, this does not apply since objects cannot be
   *  allocated contiguously. The remaining indices are treated normally.
   *  @param baseType the type of the base pointer value. This should be an addressable pointer
   *    type. An error will be returned if it is not.
   */
  def validatePointerIndices(module: Module, 
                             pointerType: Type,
                             indices: List[Value]): List[CompileException] =
  {
    indices match {
      case Nil => List(MissingElementIndexException(getLocation))
      case i :: is => {
        val firstIndexErrors = checkType(i.ty, IntType.wordType(module), getLocation)
        firstIndexErrors ++ (pointerType match {
          case PointerType(baseType, _) => validateIndices(module, baseType, is)
          case ClassType(className, typeArguments, _) => {
            val wordSize = IntType.wordSize(module)
            val clas = module.getClass(className)
            i match {
              case IntValue(ix, wordSize) if 0 <= ix && ix < clas.fields.size => {
                val field = module.getField(clas.fields(ix.toInt))
                val fieldType = field.ty.substitute(clas.typeParameters, typeArguments)
                validateIndices(module, field.ty, is)
              }
              case _ => List(InvalidIndexException(i.toString, pointerType.toString, getLocation))
            }
          }
          case _ => List(TypeMismatchException(pointerType.toString, "addressable pointer type", getLocation))
        })
      }
    }
  }
}

final case class AddressInstruction(name: Symbol,
                                    ty: Type,
                                    base: Value,
                                    indices: List[Value],
                                    annotations: List[AnnotationValue] = Nil)
  extends Instruction 
  with PointerElementInstruction
{
  def operands = base :: indices

  override def validate(module: Module) = {
    super.validate(module) ++
      stage(validatePointerIndices(module, base.ty, indices),
            checkType(getPointerType(module, base.ty, indices), ty, getLocation))
  }
}

sealed abstract class BinaryOperator(val name: String) {
  def isArithmetic: Boolean = false
  def isShift: Boolean = false
  def isLogical: Boolean = false
  override def equals(that: Any) = {
    that match {
      case BinaryOperator(n) if name == n => true
      case _ => false
    }
  }
  override def hashCode = hash("BinaryOperator", name)
  override def toString = "BinaryOperator(" + name + ")"
}
object BinaryOperator {
  def unapply(that: Any): Option[String] = {
    if (that.isInstanceOf[BinaryOperator])
      Some(that.asInstanceOf[BinaryOperator].name)
    else
      None
  }

  val MULTIPLY = new ArithmeticOperator("*")
  val DIVIDE = new ArithmeticOperator("/")
  val REMAINDER = new ArithmeticOperator("%")
  val ADD = new ArithmeticOperator("+")
  val SUBTRACT = new ArithmeticOperator("-")
  val LEFT_SHIFT = new ShiftOperator("<<")
  val RIGHT_SHIFT_ARITHMETIC = new ShiftOperator(">>")
  val RIGHT_SHIFT_LOGICAL = new ShiftOperator(">>>")
  val AND = new LogicalOperator("&")
  val XOR = new LogicalOperator("^")
  val OR = new LogicalOperator("|")

  def fromString(name: String) = {
    name match {
      case "*" => MULTIPLY
      case "/" => DIVIDE
      case "%" => REMAINDER
      case "+" => ADD
      case "-" => SUBTRACT
      case "<<" => LEFT_SHIFT
      case ">>" => RIGHT_SHIFT_ARITHMETIC
      case ">>>" => RIGHT_SHIFT_LOGICAL
      case "&" => AND
      case "^" => XOR
      case "|" => OR
      case _ => throw new RuntimeException("invalid binary operator")
    }
  }
}

final class ArithmeticOperator(name: String) extends BinaryOperator(name) {
  override def isArithmetic = true
}
final class ShiftOperator(name: String) extends BinaryOperator(name) {
  override def isShift = true
}
final class LogicalOperator(name: String) extends BinaryOperator(name) {
  override def isLogical = true
}

final case class BinaryOperatorInstruction(name: Symbol,
                                           ty: Type,
                                           operator: BinaryOperator,
                                           left: Value,
                                           right: Value,
                                           annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(left, right)

  override def validate(module: Module) = {
    def validateOperation = { 
      val lty = left.ty
      val rty = right.ty
      if (!lty.supportsOperator(operator))
        List(new UnsupportedNumericOperationException(lty, operator.toString, getLocation))
      else if (lty != rty)
        List(TypeMismatchException(rty.toString, lty.toString, getLocation))
      else
        Nil
    }

    stage(super.validate(module) ++ validateOperation,
          checkType(left.ty, ty, getLocation))
  }
}

final case class BitCastInstruction(name: Symbol,
                                    ty: Type,
                                    value: Value,
                                    annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  override def validate(module: Module) = {
    def validateCast = {
      val valueSize = value.ty.size(module)
      val tySize = ty.size(module)
      if (valueSize != tySize)
        List(InvalidBitCastException(value, valueSize, ty, tySize, getLocation))
      else
        Nil
    }
    stage(super.validate(module),
          validateCast)
  }
}

final case class BranchInstruction(name: Symbol, 
                                   ty: Type,
                                   target: Symbol,
                                   arguments: List[Value],
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
  with CallInstruction
{
  override def isTerminating = true

  override def successors = Set(target)

  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def liveOutBindings: Map[Symbol, List[Value]] = Map(target -> arguments)

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentOfClass[Block](module, target)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    val errors = super.validateScope(module, scope)
    validateSymbolScope(errors, target, scope)
  }

  override def validate(module: Module) = {
    val block = module.getBlock(target)
    val parameters = module.getParameters(block.parameters)
    val parameterTypes = parameters.map(_.ty)
    super.validate(module) ++ 
      validateCall(module, target, block.ty(module), Nil, arguments, ty)
  }
}

final case class CatchInstruction(name: Symbol,
                                  ty: Type,
                                  annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = Nil

  override def validate(module: Module) = {
    super.validate(module) ++ checkNonNullPointerType(ty, getLocation)
  }
}

final case class ConditionalBranchInstruction(name: Symbol,
                                              ty: Type,
                                              condition: Value,
                                              trueTarget: Symbol,
                                              trueArguments: List[Value],
                                              falseTarget: Symbol,
                                              falseArguments: List[Value],
                                              annotations: List[AnnotationValue] = Nil)
  extends Instruction 
  with CallInstruction
{
  override def isTerminating = true

  override def successors = Set(trueTarget, falseTarget)

  def operands = condition :: trueArguments ++ falseArguments

  override def usedSymbols = trueTarget :: falseTarget :: operandSymbols

  override def liveOutBindings: Map[Symbol, List[Value]] = {
    Map(trueTarget  -> trueArguments,
        falseTarget -> falseArguments)
  }

  override def validateComponents(module: Module) = {
    // need to validate these separately because they are allowed to be duplicates
    super.validateComponents(module) ++
      validateComponentOfClass[Block](module, trueTarget) ++
      validateComponentOfClass[Block](module, falseTarget)
  }

  override def validateScope(module: Module, scope: Set[Symbol]) = {
    var errors = super.validateScope(module, scope)
    errors = validateSymbolScope(errors, trueTarget, scope)
    errors = validateSymbolScope(errors, falseTarget, scope)
    errors
  }

  override def validate(module: Module) = {
    // We assume that both of the branch targets refer to local blocks and that the block
    // parameters are validated. This should be done by validateScope

    def validateBranch(target: Symbol, arguments: List[Value]) = {
      val block = module.getBlock(target)
      val parameterTypes = module.getParameters(block.parameters).map(_.ty)
      validateCall(module, target, block.ty(module), Nil, arguments, ty)
    }

    stage(super.validate(module),
          validateBranch(trueTarget, trueArguments),
          validateBranch(falseTarget, falseArguments),
          checkType(condition.ty, BooleanType, getLocation))
  }
}

final case class ExtractInstruction(name: Symbol,
                                    ty: Type,
                                    base: Value,
                                    indices: List[Value],
                                    annotations: List[AnnotationValue] = Nil)
  extends Instruction
  with ElementInstruction
{
  def operands = base :: indices

  override def validate(module: Module) = {
    def typeIsValid = {
      val elementType = getElementType(module, base.ty, indices)
      checkType(ty, elementType, getLocation)
    }
    super.validate(module) ++
      stage(validateIndices(module, base.ty, indices),
            typeIsValid)
  }
}

sealed abstract class FloatCastInstruction(name: Symbol,
                                           ty: Type,
                                           value: Value,
                                           annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  protected def validateWidths(fromTy: FloatType, toTy: FloatType): List[CompileException]

  override def validate(module: Module) = {
    def validateCast = { 
      (value.ty, ty) match {
        case (fromTy: FloatType, toTy: FloatType) => validateWidths(fromTy, toTy)
        case (fromTy, _: FloatType) => {
          List(TypeMismatchException(fromTy.toString, "float type", getLocation))
        }
        case (_, toTy) => List(TypeMismatchException(toTy.toString, "float type", getLocation))
      }
    }

    super.validate(module) ++ validateCast    
  }
}

final case class FloatExtendInstruction(name: Symbol,
                                        ty: Type,
                                        value: Value,
                                        annotations: List[AnnotationValue] = Nil)
  extends FloatCastInstruction(name, ty, value, annotations)
{
  protected def validateWidths(fromTy: FloatType, toTy: FloatType) = {
    if (toTy.width > fromTy.width)
      Nil
    else
      List(NumericExtensionException(fromTy.toString, toTy.toString, getLocation))
  }
}

final case class FloatToIntegerInstruction(name: Symbol,
                                           ty: Type,
                                           value: Value,
                                           annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  override def validate(module: Module) = {
    def validateCast = { 
      val fromTy = value.ty
      val fromTyErrors = if (!fromTy.isInstanceOf[FloatType])
        List(TypeMismatchException(fromTy.toString, "float type", getLocation))
      else
        Nil
      val toTyErrors = if (!ty.isInstanceOf[IntType])
        List(TypeMismatchException(ty.toString, "integer type", getLocation))
      else
        Nil
      fromTyErrors ++ toTyErrors
    }

    super.validate(module) ++ validateCast
  }
}

final case class FloatTruncateInstruction(name: Symbol,
                                          ty: Type,
                                          value: Value,
                                          annotations: List[AnnotationValue] = Nil)
  extends FloatCastInstruction(name, ty, value, annotations)
{
  protected def validateWidths(fromTy: FloatType, toTy: FloatType) = {
    if (toTy.width < fromTy.width)
      Nil
    else
      List(NumericTruncationException(fromTy.toString, toTy.toString, getLocation))
  }
}

final case class HeapAllocateInstruction(name: Symbol,
                                         ty: Type,
                                         annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = Nil

  override def validate(module: Module) = {
    super.validate(module) ++ checkNonNullPointerType(ty, getLocation)
  }
}

final case class HeapAllocateArrayInstruction(name: Symbol,
                                              ty: Type,
                                              count: Value,
                                              annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(count)

  override def validate(module: Module) = {
    super.validate(module) ++ 
      checkType(count.ty, IntType.wordType(module), getLocation) ++
      checkNonNullPointerType(ty, getLocation)
  }
}

final case class InsertInstruction(name: Symbol,
                                   ty: Type,
                                   value: Value,
                                   base: Value,
                                   indices: List[Value],
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
  with ElementInstruction
{
  def operands = base :: value :: indices

  override def validate(module: Module) = {
    def validateValueType = {
      val elementType = getElementType(module, base.ty, indices)
      checkType(elementType, value.ty, getLocation)
    }
    stage(super.validate(module),
          checkType(ty, base.ty, getLocation),
          validateIndices(module, base.ty, indices),
          validateValueType)
  }      
}

final case class IntegerToFloatInstruction(name: Symbol,
                                           ty: Type,
                                           value: Value,
                                           annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  override def validate(module: Module) = {
    def validateCast = {
      val fromTy = value.ty
      val fromTyErrors = if (!fromTy.isInstanceOf[IntType])
        List(TypeMismatchException(fromTy.toString, "integer type", getLocation))
      else
        Nil
      val toTyErrors = if (!ty.isInstanceOf[FloatType])
        List(TypeMismatchException(ty.toString, "float type", getLocation))
      else
        Nil
      fromTyErrors ++ toTyErrors
    }
    super.validate(module) ++ validateCast
  }    
}

sealed abstract class IntegerCastInstruction(name: Symbol,
                                             ty: Type,
                                             value: Value,
                                             annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  protected def validateWidths(fromTy: IntType, toTy: IntType): List[CompileException]

  override def validate(module: Module) = {
    def validateCast = { 
      (value.ty, ty) match {
        case (fromTy: IntType, toTy: IntType) => validateWidths(fromTy, toTy)
        case (fromTy, _: IntType) => {
          List(TypeMismatchException(fromTy.toString, "integer type", getLocation))
        }
        case (_, toTy) => List(TypeMismatchException(toTy.toString, "integer type", getLocation))
      }
    }
    super.validate(module) ++ validateCast
  }
}

final case class IntegerSignExtendInstruction(name: Symbol,
                                              ty: Type,
                                              value: Value,
                                              annotations: List[AnnotationValue] = Nil)
  extends IntegerCastInstruction(name, ty, value, annotations)
{
  protected def validateWidths(fromTy: IntType, toTy: IntType) = {
    if (toTy.width >= fromTy.width)
      Nil
    else
      List(NumericExtensionException(fromTy.toString, toTy.toString, getLocation))
  }
}

final case class IntegerTruncateInstruction(name: Symbol,
                                            ty: Type,
                                            value: Value,
                                            annotations: List[AnnotationValue] = Nil)
  extends IntegerCastInstruction(name, ty, value, annotations)
{
  protected def validateWidths(fromTy: IntType, toTy: IntType) = {
    if (toTy.width < fromTy.width)
      Nil
    else
      List(NumericTruncationException(fromTy.toString, toTy.toString, getLocation))
  }      
}

final case class IntegerZeroExtendInstruction(name: Symbol,
                                              ty: Type,
                                              value: Value,
                                              annotations: List[AnnotationValue] = Nil)
  extends IntegerCastInstruction(name, ty, value, annotations)
{
  protected def validateWidths(fromTy: IntType, toTy: IntType) = {
    if (toTy.width > fromTy.width)
      Nil
    else
      List(NumericExtensionException(fromTy.toString, toTy.toString, getLocation))
  }
}

case class IntrinsicFunction(name: String,
                             ty: FunctionType)

object Intrinsic {
  val EXIT = IntrinsicFunction("exit", 
                               FunctionType(UnitType, Nil, List(IntType(32))))
  val READ = IntrinsicFunction("read", 
                               FunctionType(IntType(64), Nil,              // bytes read
                                            List(IntType(32),              // file descriptor
                                                 PointerType(IntType(8)),  // buffer
                                                 IntType(64))))            // buffer capacity
  val WRITE = IntrinsicFunction("write",
                                FunctionType(IntType(64), Nil,             // bytes written
                                             List(IntType(32),             // file descriptor
                                                  PointerType(IntType(8)), // buffer
                                                  IntType(64))))           // buffer size
  val OPEN = IntrinsicFunction("open",
                               FunctionType(IntType(32), Nil,              // file descriptor
                                            List(PointerType(IntType(8)),  // file name
                                                 IntType(32))))            // flags

  val CLOSE = IntrinsicFunction("close",
                                FunctionType(UnitType, Nil,
                                             List(IntType(32))))           // file descriptor

  val INTRINSICS = List(EXIT, READ, WRITE, OPEN, CLOSE)
}

final case class IntrinsicCallInstruction(name: Symbol,
                                          ty: Type,
                                          intrinsic: IntrinsicFunction,
                                          arguments: List[Value],
                                          annotations: List[AnnotationValue] = Nil)
  extends Instruction with CallInstruction
{
  def operands = arguments

  override def validate(module: Module) = {
    super.validate(module) ++ 
      validateCall(module, Symbol(intrinsic.name), intrinsic.ty, Nil, arguments, ty)
  }
}

final case class LoadInstruction(name: Symbol,
                                 ty: Type,
                                 pointer: Value,
                                 annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(pointer)

  override def validate(module: Module) = {
    def typeIsValid = {
      val PointerType(elementType, _) = pointer.ty
      checkType(elementType, ty, getLocation)
    }

    super.validate(module) ++ 
      stage(checkNonNullPointerType(pointer.ty, getLocation),
            typeIsValid)
  }
}

final case class LoadElementInstruction(name: Symbol,
                                        ty: Type,
                                        base: Value,
                                        indices: List[Value],
                                        annotations: List[AnnotationValue] = Nil)
  extends Instruction with PointerElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: indices

  override def validate(module: Module) = {
    def validateType = {
      val PointerType(elementType, _) = getPointerType(module, base.ty, indices)
      checkType(elementType, ty, getLocation)
    }

    super.validate(module) ++
      stage(validatePointerIndices(module, base.ty, indices),
            validateType)
  }
}    

final case class NewInstruction(name: Symbol,
                                ty: Type,
                                constructorName: Symbol,
                                typeArguments: List[Type],
                                arguments: List[Value],
                                annotations: List[AnnotationValue] = Nil)
  extends Instruction with CallInstruction
{
  def operands = arguments

  override def usedSymbols = constructorName :: operandSymbols

  override def validateComponents(module: Module) = {
    def validateType = {
      if (ty.isInstanceOf[ClassType])
        Nil
      else
        List(TypeMismatchException(ty, "class type", getLocation))
    }

    super.validateComponents(module) ++
      validateType ++
      validateComponentOfClass[Function](module, constructorName)
  }

  override def validate(module: Module) = {
    val classType = ty.asInstanceOf[ClassType]
    val ctor = module.getFunction(constructorName)
    val ctorType = ctor.ty(module)
    val newType = ctorType.copy(returnType = classType)
    val clas = module.getClass(classType.className)
    val thisValue = makeValue

    def validateAbstract = {
      if (clas.isAbstract)
        List(NewAbstractException(name, clas.name, getLocation))
      else
        Nil
    }

    def validateConstructor = {
      if (clas.constructors.contains(constructorName))
        Nil
      else
        List(NewConstructorException(name, constructorName, clas.name, getLocation))
    }

    super.validate(module) ++
      stage(validateAbstract,
            validateConstructor,
            validateCall(module, 
                         constructorName, 
                         newType, 
                         classType.typeArguments ++ typeArguments, 
                         thisValue :: arguments,
                         classType))
  }
}

final case class NullCheckInstruction(name: Symbol,
                                      ty: Type,
                                      value: Value,
                                      annotations: List[AnnotationValue] = Nil)
extends Instruction
{
  def operands = List(value)

  override def validate(module: Module): List[CompileException] = {
    ty match {
      case toTy: ReferenceType if !toTy.isNullable => {
        val expectedFromTy = toTy.setPointerFlags(ReferenceType.NULLABLE)
        value.ty match {
          case fromTy: ReferenceType if fromTy.isNullable && fromTy == expectedFromTy => Nil
          case _ => List(TypeMismatchException(expectedFromTy, value.ty, getLocation))
        }
      }
      case _ => List(TypeMismatchException("non-nullable reference type", ty, getLocation))
    }
  }
}

final case class PointerCallInstruction(name: Symbol,
                                        ty: Type,
                                        target: Value,
                                        typeArguments: List[Type],
                                        arguments: List[Value],
                                        annotations: List[AnnotationValue] = Nil)
extends Instruction
  with CallInstruction
{
  def operands = target :: arguments

  override def validate(module: Module) = {
    val callErrors = target.ty match {
      case targetType: FunctionType => {
        // We assume the target is a defined value. This must be true if there is no 
        // specific "function" value. 
        val targetName = target.asInstanceOf[DefinedValue].value
        validateCall(module, targetName, targetType, typeArguments, arguments, ty)
      }
      case _ => List(TypeMismatchException(target.ty, "function type", getLocation))
    }

    super.validate(module) ++ callErrors
  }
}

final case class RelationalOperator(name: String)

object RelationalOperator {
  val LESS_THAN = RelationalOperator("<")
  val LESS_EQUAL = RelationalOperator("<=")
  val GREATER_THAN = RelationalOperator(">")
  val GREATER_EQUAL = RelationalOperator(">=")
  val EQUAL = RelationalOperator("==")
  val NOT_EQUAL = RelationalOperator("!=")

  def fromString(name: String) = {
    name match {
      case "<" => LESS_THAN
      case "<=" => LESS_EQUAL
      case ">" => GREATER_THAN
      case ">=" => GREATER_EQUAL
      case "==" => EQUAL
      case "!=" => NOT_EQUAL
    }
  }
}

final case class RelationalOperatorInstruction(name: Symbol,
                                               ty: Type,
                                               operator: RelationalOperator,
                                               left: Value,
                                               right: Value,
                                               annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(left, right)

  override def validate(module: Module) = {
    def validateOperation = {
      val lty = left.ty
      val rty = right.ty
      if (!lty.supportsOperator(operator))
        List(new UnsupportedNumericOperationException(lty, operator, getLocation))
      else if (lty != rty)
        List(TypeMismatchException(rty.toString, lty.toString, getLocation))
      else
        Nil
    }
    super.validate(module) ++ validateOperation ++ checkType(BooleanType, ty, getLocation)
  }
}

final case class ReturnInstruction(name: Symbol,
                                   ty: Type,
                                   value: Value,
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  override def isTerminating = true

  override def validate(module: Module) = {
    super.validate(module) ++ checkType(ty, UnitType, getLocation)
  }
}

final case class StackAllocateInstruction(name: Symbol,
                                          ty: Type,
                                          annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = Nil

  override def validate(module: Module) = {
    super.validate(module) ++ checkNonNullPointerType(ty, getLocation)
  }
}

final case class StackAllocateArrayInstruction(name: Symbol,
                                               ty: Type,
                                               count: Value,
                                               annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(count)

  override def validate(module: Module) = {
    super.validate(module) ++
      checkType(count.ty, IntType.wordType(module), getLocation) ++
      checkNonNullPointerType(ty, getLocation)
  }
}

final case class StaticCallInstruction(name: Symbol,
                                       ty: Type,
                                       target: Symbol,
                                       typeArguments: List[Type],
                                       arguments: List[Value],
                                       annotations: List[AnnotationValue] = Nil)
  extends Instruction with CallInstruction
{
  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentOfClass[Function](module, target)
  }

  override def validate(module: Module) = {
    val targetType = module.getFunction(target).ty(module)
    super.validate(module) ++ 
      validateCall(module, target, targetType, typeArguments, arguments, ty)
  }

  private def targetName = target

  private def targetType(module: Module) = module.getFunction(target).ty(module)
}

final case class StoreInstruction(name: Symbol,
                                  ty: Type,
                                  value: Value,
                                  pointer: Value,
                                  annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(pointer, value)

  override def validate(module: Module) = {
    def validateTypes = {
      val pointerType = pointer.ty
      pointerType match {
        case PointerType(elementType, _) => {
          val valueType = value.ty
          if (valueType != elementType)
            List(TypeMismatchException(valueType.toString, elementType.toString, getLocation))
          else
            Nil
        }
        case _ => 
          List(TypeMismatchException(pointerType.toString, "non-null pointer type", getLocation))
      }
    }
    super.validate(module) ++
      stage(checkNonNullPointerType(pointer.ty, getLocation),
            checkType(PointerType(value.ty), pointer.ty, getLocation),
            checkType(ty, UnitType, getLocation))
  }
}

final case class StoreElementInstruction(name: Symbol,
                                         ty: Type,
                                         value: Value,
                                         base: Value,
                                         indices: List[Value],
                                         annotations: List[AnnotationValue] = Nil)
  extends Instruction
  with PointerElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: value :: indices

  override def validate(module: Module) = {
    def validateType = {
      val PointerType(elementType, _) = getPointerType(module, base.ty, indices)
      checkType(value.ty, elementType, getLocation)
    }

    super.validate(module) ++
      stage(validatePointerIndices(module, base.ty, indices),
            validateType,
            checkType(UnitType, ty, getLocation))
  }
}

final case class ThrowInstruction(name: Symbol,
                                  ty: Type,
                                  value: Value,
                                  annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  override def isTerminating = true

  override def validate(module: Module) = {
    def validateType = {
      if (value.ty.isInstanceOf[ObjectDefinitionType])
        Nil
      else
        List(TypeMismatchException(value.ty, "class or interface type", getLocation))
    }

    super.validate(module) ++
      validateType ++
      checkType(ty, UnitType, getLocation)
  }
}

final case class UnreachableInstruction(name: Symbol,
                                        ty: Type,
                                        annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = Nil

  override def isTerminating = true

  override def validate(module: Module) = {
    checkType(ty, UnitType, getLocation)
  }
}

final case class UpcastInstruction(name: Symbol,                
                                   ty: Type,
                                   value: Value,
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  override def validate(module: Module) = {
    def validateCast = {
      val valueTy = value.ty
      if (valueTy.isSubtypeOf(ty, module))
        Nil
      else
        List(UpcastException(valueTy.toString, ty.toString, getLocation))
    }
    super.validate(module) ++ validateCast
  }
}

final case class VirtualCallInstruction(name: Symbol,
                                        ty: Type,
                                        target: Value,
                                        methodIndex: Int,
                                        typeArguments: List[Type],
                                        arguments: List[Value],
                                        annotations: List[AnnotationValue] = Nil)
  extends Instruction
  with CallInstruction
{
  def operands = target :: arguments

  override def validate(module: Module) = {
    def validateVirtualCall = {
      target.ty match {
        case targetType: ObjectDefinitionType => {
          val fullTypeArguments = targetType.typeArguments ++ typeArguments
          val definition = targetType.getObjectDefinition(module)
          definition.methods.lift(methodIndex) match {
            case Some(methodName) => {
              val method = module.getFunction(methodName)
              val methodType = method.ty(module)
              val methodTypeWithoutThis = methodType.copy(parameterTypes = methodType.parameterTypes.tail)
              validateCall(module, method.name, methodTypeWithoutThis, 
                           fullTypeArguments, arguments, ty)
            }
            case None => List(InvalidVirtualMethodIndexException(methodIndex, ty, getLocation))
          }
        }
        case _ => List(TypeMismatchException(ty, "class or interface type", getLocation))
      }
    }

    super.validate(module) ++ validateVirtualCall
  }
}

final case class VirtualLookupInstruction(name: Symbol,
                                          ty: Type,
                                          obj: Value,
                                          methodIndex: Int,
                                          annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(obj)

  override def validate(module: Module) = {
    def validateLookup = {
      obj.ty match {
        case objectType: ObjectDefinitionType => {
          val definition = objectType.getObjectDefinition(module)
          definition.methods.lift(methodIndex) match {
            case Some(methodName) => {
              val method = module.getFunction(methodName)
              val methodType = method.ty(module)
              if (methodType != ty)
                List(TypeMismatchException(methodType, ty, getLocation))
              else
                Nil
            }
            case None => List(InvalidVirtualMethodIndexException(methodIndex, objectType, getLocation))
          }
        }
        case _ => List(TypeMismatchException(ty, "class or interface type", getLocation))
      }
    }

    super.validate(module) ++ validateLookup
  }
}

