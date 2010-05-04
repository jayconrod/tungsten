package tungsten

import Utilities._

sealed abstract class Instruction
  extends Definition
{
  def ty: Type

  def ty(module: Module): Type

  def isTerminating = false

  def operands: List[Value]

  /** Collects symbols used by operands. This does not count symbols used inside types, only
   *  names of instructions, parameters, and globals referenced.
   */
  def operandSymbols = {
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

  override def validateComponents(module: Module): List[CompileException] = {
    super.validateComponents(module) ++
      operands.flatMap(_.validateComponents(module, getLocation))
  }

  override def validate(module: Module): List[CompileException] = {
    super.validate(module) ++
      operands.flatMap(_.validate(module, getLocation))
  }

  protected final def validateOperands(module: Module) = {
    operands.flatMap(_.validateComponents(module, getLocation))
  }
}

trait CallInstruction extends Instruction {
  protected def validateCall(module: Module,
                             targetName: Symbol,
                             parameterTypes: List[Type],
                             arguments: List[Value]): List[CompileException] = 
  {
    if (arguments.size != parameterTypes.size) {
      List(FunctionArgumentCountException(targetName, 
                                          arguments.size,
                                          parameterTypes.size,
                                          getLocation))
    } else {
      (arguments zip parameterTypes) flatMap { at => 
        val (a, t) = at
        checkType(a.ty, t, getLocation)
      }
    }
  }
}   

sealed trait ElementInstruction extends Instruction {
  protected final def getElementType(module: Module,
                                     baseType: Type, 
                                     indices: List[Value]): Type =                                     
  {
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
                val field = module.getField(struct.fields(ix.asInstanceOf[Int]))
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

  protected final def validateIndices(module: Module,
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
                  val field = module.getField(struct.fields(ix.asInstanceOf[Int]))
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

final case class AddressInstruction(name: Symbol,
                                    ty: Type,
                                    base: Value,
                                    indices: List[Value],
                                    annotations: List[AnnotationValue] = Nil)
  extends Instruction 
  with ElementInstruction
{
  def ty(module: Module) = {
    base.ty match {
      case PointerType(elementType) => {
        val resultElementType = getElementType(module, elementType, indices)
        PointerType(resultElementType)
      }
      case _ => PointerType(UnitType) // bogus, but we catch it in validation
    }
  }

  def operands = base :: indices

  override def validate(module: Module) = {
    def indicesAreValid = {
      val PointerType(ptrElementType) = base.ty
      validateIndices(module, ptrElementType, indices)
    }

    def tyIsValid = {
      val PointerType(ptrElementType) = base.ty
      val calculatedType = PointerType(getElementType(module, ptrElementType, indices))
      checkType(ty(module), calculatedType, getLocation)
    }      

    super.validate(module) ++
      stage(checkNonNullPointerType(base.ty, getLocation),
            indicesAreValid,
            tyIsValid)
  }
}

final case class AssignInstruction(name: Symbol,
                                   ty: Type,
                                   value: Value,
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  def ty(module: Module) = value.ty
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

  def ty(module: Module) = left.ty

  override def validate(module: Module) = {
    def validateOperation = { 
      val lty = left.ty
      val rty = right.ty
      if (!lty.supportsOperator(operator))
        List(UnsupportedNumericOperationException(lty, operator, getLocation))
      else if (lty != rty)
        List(TypeMismatchException(rty.toString, lty.toString, getLocation))
      else
        Nil
    }

    super.validate(module) ++ validateOperation
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

  def ty(module: Module) = UnitType

  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentOfClass[Block](module, target)
  }

  override def validate(module: Module) = {
    val block = module.getBlock(target)
    val parameters = module.getParameters(block.parameters)
    val parameterTypes = parameters.map(_.ty)
    super.validate(module) ++ 
      validateCall(module, target, parameterTypes, arguments)
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
  def ty(module: Module) = UnitType

  override def isTerminating = true

  def operands = condition :: trueArguments ++ falseArguments

  override def usedSymbols = trueTarget :: falseTarget :: operandSymbols

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Block](module, List(trueTarget, falseTarget))
  }

  override def validate(module: Module) = {
    // We assume that both of the branch targets refer to local blocks and that the block
    // parameters are validated. This should be done by Block and Function validation

    def validateBranch(target: Symbol, arguments: List[Value]) = {
      val block = module.getBlock(target)
      val parameterTypes = module.getParameters(block.parameters).map(_.ty)
      validateCall(module, target, parameterTypes, arguments)
    }

    stage(super.validate(module),
          validateBranch(trueTarget, trueArguments),
          validateBranch(falseTarget, falseArguments),
          checkType(condition.ty, BooleanType, getLocation))
  }
}

sealed abstract class FloatCastInstruction(name: Symbol,
                                           ty: Type,
                                           value: Value,
                                           annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def ty(module: Module): Type = ty

  def operands = List(value)

  protected def validateWidths(fromTy: FloatType, toTy: FloatType): List[CompileException]

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          ty.validate(module, getLocation))
  }

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
  def ty(module: Module): Type = ty

  def operands = List(value)

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation)
  }

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
  def ty(module: Module): Type = ty

  def operands = Nil

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation)
  }

  override def validate(module: Module) = {
    super.validate(module) ++ checkNonNullPointerType(ty, getLocation)
  }
}

final case class HeapAllocateArrayInstruction(name: Symbol,
                                              ty: Type,
                                              count: Value,
                                              elementType: Type,
                                              annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def ty(module: Module) = PointerType(ArrayType(None, elementType))

  def operands = List(count)

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      elementType.validate(module, getLocation)
  }

  override def validate(module: Module) = {
    super.validate(module) ++ checkType(count.ty, IntType.wordType(module), getLocation)
  }
}

final case class IntegerToFloatInstruction(name: Symbol,
                                           ty: Type,
                                           value: Value,
                                           annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def ty(module: Module): Type = ty

  def operands = List(value)

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation)
  }

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
  def ty(module: Module): Type = ty

  def operands = List(value)

  protected def validateWidths(fromTy: IntType, toTy: IntType): List[CompileException]

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation)
  }

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

case class IntrinsicFunction(number: Int, 
                             name: String,
                             ty: FunctionType)

object Intrinsic {
  val EXIT = IntrinsicFunction(1, "exit", FunctionType(UnitType, List(IntType(32))))
  val INTRINSICS = List(EXIT)
}

final case class IntrinsicCallInstruction(name: Symbol,
                                          ty: Type,
                                          intrinsic: IntrinsicFunction,
                                          arguments: List[Value],
                                          annotations: List[AnnotationValue] = Nil)
  extends Instruction with CallInstruction
{
  def ty(module: Module) = intrinsic.ty.returnType

  override def isTerminating = intrinsic == Intrinsic.EXIT

  def operands = arguments

  override def validate(module: Module) = {
    super.validate(module) ++ 
      validateCall(module, Symbol(intrinsic.name), intrinsic.ty.parameterTypes, arguments)
  }
}

final case class LoadInstruction(name: Symbol,
                                 ty: Type,
                                 pointer: Value,
                                 annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(pointer)

  def ty(module: Module) = {
    val pointerType = pointer.ty.asInstanceOf[PointerType]
    pointerType.elementType
  }
  
  override def validate(module: Module) = {
    super.validate(module) ++ checkNonNullPointerType(pointer.ty, getLocation)
  }
}

final case class LoadElementInstruction(name: Symbol,
                                        ty: Type,
                                        base: Value,
                                        indices: List[Value],
                                        annotations: List[AnnotationValue] = Nil)
  extends Instruction with ElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: indices

  def ty(module: Module) = getElementType(module, base.ty, indices)

  override def validate(module: Module) = {
    super.validate(module) ++ validateIndices(module, base.ty, indices)
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

  def ty(module: Module) = BooleanType

  override def validate(module: Module) = {
    def validateOperation = {
      val lty = left.ty
      val rty = right.ty
      if (!lty.supportsOperator(operator))
        List(UnsupportedNumericOperationException(lty, operator, getLocation))
      else if (lty != rty)
        List(TypeMismatchException(rty.toString, lty.toString, getLocation))
      else
        Nil
    }
    super.validate(module) ++ validateOperation
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

  def ty(module: Module) = UnitType
}

final case class StoreInstruction(name: Symbol,
                                  ty: Type,
                                  value: Value,
                                  pointer: Value,
                                  annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(pointer, value)

  def ty(module: Module) = UnitType

  override def validate(module: Module) = {
    def validateTypes = {
      val pointerType = pointer.ty
      pointerType match {
        case PointerType(elementType) => {
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
            checkType(PointerType(value.ty), pointer.ty, getLocation))
  }
}

final case class StoreElementInstruction(name: Symbol,
                                         ty: Type,
                                         value: Value,
                                         base: Value,
                                         indices: List[Value],
                                         annotations: List[AnnotationValue] = Nil)
  extends Instruction
  with ElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: value :: indices

  def ty(module: Module) = UnitType

  override def validate(module: Module) = {
    def validateValueType = {
      val elementType = getElementType(module, base.ty, indices)
      checkType(value.ty, elementType, getLocation)
    }
    super.validate(module) ++ 
      stage(validateIndices(module, base.ty, indices),
            validateValueType)
  }
}

final case class StackAllocateInstruction(name: Symbol,
                                          ty: Type,
                                          annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def ty(module: Module): Type = ty

  def operands = Nil

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation)
  }

  override def validate(module: Module) = {
    super.validate(module) ++ checkNonNullPointerType(ty, getLocation)
  }
}

final case class StackAllocateArrayInstruction(name: Symbol,
                                               ty: Type,
                                               count: Value,
                                               elementType: Type,
                                               annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def ty(module: Module) = PointerType(ArrayType(None, elementType))

  def operands = List(count)

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      elementType.validate(module, getLocation)
  }

  override def validate(module: Module) = {
    super.validate(module) ++ 
      checkType(count.ty, IntType.wordType(module), getLocation)
  }
}

final case class StaticCallInstruction(name: Symbol,
                                       ty: Type,
                                       target: Symbol,
                                       arguments: List[Value],
                                       annotations: List[AnnotationValue] = Nil)
  extends Instruction with CallInstruction
{
  def ty(module: Module) = targetType(module).returnType

  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentOfClass[Function](module, target)
  }

  override def validate(module: Module) = {
    super.validate(module) ++ 
      validateCall(module, target, targetType(module).parameterTypes, arguments)
  }

  private def targetName = target

  private def targetType(module: Module) = module.getFunction(target).ty(module)
}

final case class UpcastInstruction(name: Symbol,                
                                   ty: Type,
                                   value: Value,
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  def ty(module: Module): Type = ty

  override def validate(module: Module) = {
    def validateCast = {
      val valueTy = value.ty
      if (valueTy <<: ty)
        Nil
      else
        List(UpcastException(valueTy.toString, ty.toString, getLocation))
    }
    super.validate(module) ++ validateCast
  }
}
