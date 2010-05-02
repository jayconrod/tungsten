package tungsten

import Utilities._

sealed abstract class Instruction
  extends Definition
{
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
      validateOperands(module)
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
        a.validateType(t, module, getLocation)
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
              case _ => UnitType
            }
          }
          case _ => UnitType  // bogus, but we'll catch it in validation
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
          val indexType = i.ty(module)
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
                                    base: Value,
                                    indices: List[Value],
                                    annotations: List[AnnotationValue] = Nil)
  extends Instruction 
  with ElementInstruction
{
  def ty(module: Module) = {
    base.ty(module) match {
      case PointerType(elementType) => {
        val resultElementType = getElementType(module, elementType, indices)
        PointerType(resultElementType)
      }
      case _ => PointerType(UnitType) // bogus, but we catch it in validation
    }
  }

  def operands = base :: indices

  override def validate(module: Module) = {
    val baseType = base.ty(module)
    def baseIsPointer = {
      if (baseType.isInstanceOf[PointerType])
        Nil
      else
        List(TypeMismatchException(baseType.toString, "non-null pointer type", getLocation))
    }

    def indicesAreValid = {
      val elementType = baseType.asInstanceOf[PointerType].elementType
      validateIndices(module, elementType, indices)
    }

    stage(super.validate(module),
          baseIsPointer,
          indicesAreValid)
  }
}

final case class AssignInstruction(name: Symbol,
                                   value: Value,
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  def ty(module: Module) = value.ty(module)
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
                                           operator: BinaryOperator,
                                           left: Value,
                                           right: Value,
                                           annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(left, right)

  def ty(module: Module) = left.ty(module)

  override def validate(module: Module) = {
    def validateOperation = { 
      val lty = left.ty(module)
      val rty = right.ty(module)
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
          condition.validateType(BooleanType, module, getLocation))
  }
}

sealed abstract class FloatCastInstruction(name: Symbol,
                                           value: Value,
                                           ty: Type,
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
      (value.ty(module), ty) match {
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
                                        value: Value,
                                        ty: Type,
                                        annotations: List[AnnotationValue] = Nil)
  extends FloatCastInstruction(name, value, ty, annotations)
{
  protected def validateWidths(fromTy: FloatType, toTy: FloatType) = {
    if (toTy.width > fromTy.width)
      Nil
    else
      List(NumericExtensionException(fromTy.toString, toTy.toString, getLocation))
  }
}

final case class FloatToIntegerInstruction(name: Symbol,
                                           value: Value,
                                           ty: Type,
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
      val fromTy = value.ty(module)
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
                                          value: Value,
                                          ty: Type,
                                          annotations: List[AnnotationValue] = Nil)
  extends FloatCastInstruction(name, value, ty, annotations)
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
    def validateType = {
      if (!ty.isPointer || ty == NullType)
        List(TypeMismatchException(ty.toString, "non-null pointer type", getLocation))
      else
        Nil
    }
    super.validate(module) ++ validateType
  }
}

final case class HeapAllocateArrayInstruction(name: Symbol,
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
    super.validate(module) ++ count.validateType(IntType.wordType(module), module, getLocation)
  }
}

final case class IntegerToFloatInstruction(name: Symbol,
                                           value: Value,
                                           ty: Type,
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
      val fromTy = value.ty(module)
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
                                             value: Value,
                                             ty: Type,
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
      (value.ty(module), ty) match {
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
                                              value: Value,
                                              ty: Type,
                                              annotations: List[AnnotationValue] = Nil)
  extends IntegerCastInstruction(name, value, ty, annotations)
{
  protected def validateWidths(fromTy: IntType, toTy: IntType) = {
    if (toTy.width >= fromTy.width)
      Nil
    else
      List(NumericExtensionException(fromTy.toString, toTy.toString, getLocation))
  }
}

final case class IntegerTruncateInstruction(name: Symbol,
                                            value: Value,
                                            ty: Type,
                                            annotations: List[AnnotationValue] = Nil)
  extends IntegerCastInstruction(name, value, ty, annotations)
{
  protected def validateWidths(fromTy: IntType, toTy: IntType) = {
    if (toTy.width < fromTy.width)
      Nil
    else
      List(NumericTruncationException(fromTy.toString, toTy.toString, getLocation))
  }      
}

final case class IntegerZeroExtendInstruction(name: Symbol,
                                              value: Value,
                                              ty: Type,
                                              annotations: List[AnnotationValue] = Nil)
  extends IntegerCastInstruction(name, value, ty, annotations)
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
                                 pointer: Value,
                                 annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(pointer)

  def ty(module: Module) = {
    val pointerType = pointer.ty(module).asInstanceOf[PointerType]
    pointerType.elementType
  }
  
  override def validate(module: Module) = {
    def validateType = {
      val pointerType = pointer.ty(module)
      if (!pointerType.isInstanceOf[PointerType])
        List(TypeMismatchException(pointerType.toString, "non-null pointer type", getLocation))
      else
        Nil
    }
    super.validate(module) ++ validateType
  }
}

final case class LoadElementInstruction(name: Symbol,
                                        base: Value,
                                        indices: List[Value],
                                        annotations: List[AnnotationValue] = Nil)
  extends Instruction with ElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: indices

  def ty(module: Module) = getElementType(module, base.ty(module), indices)

  override def validate(module: Module) = {
    super.validate(module) ++ validateIndices(module, base.ty(module), indices)
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
      val lty = left.ty(module)
      val rty = right.ty(module)
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
                                   value: Value,
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  override def isTerminating = true

  def ty(module: Module) = UnitType
}

final case class StoreInstruction(name: Symbol,
                                  value: Value,
                                  pointer: Value,
                                  annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(pointer, value)

  def ty(module: Module) = UnitType

  override def validate(module: Module) = {
    def validateTypes = {
      val pointerType = pointer.ty(module)
      pointerType match {
        case PointerType(elementType) => {
          val valueType = value.ty(module)
          if (valueType != elementType)
            List(TypeMismatchException(valueType.toString, elementType.toString, getLocation))
          else
            Nil
        }
        case _ => 
          List(TypeMismatchException(pointerType.toString, "non-null pointer type", getLocation))
      }
    }
    super.validate(module) ++ validateTypes
  }
}

final case class StoreElementInstruction(name: Symbol,
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
      val elementType = getElementType(module, base.ty(module), indices)
      value.validateType(elementType, module, getLocation)
    }
    super.validate(module) ++ 
      validateIndices(module, base.ty(module), indices) ++ 
      validateValueType
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
    def validateType = {
      if (!ty.isPointer || ty == NullType)
        List(TypeMismatchException(ty.toString, "non-null pointer type", getLocation))
      else
        Nil
    }
    super.validate(module) ++ validateType
  }
}

final case class StackAllocateArrayInstruction(name: Symbol,
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
      count.validateType(IntType(64), module, getLocation)
  }
}

final case class StaticCallInstruction(name: Symbol,
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
                                   value: Value,
                                   ty: Type,
                                   annotations: List[AnnotationValue] = Nil)
  extends Instruction
{
  def operands = List(value)

  def ty(module: Module): Type = ty

  override def validate(module: Module) = {
    def validateCast = {
      val valueTy = value.ty(module)
      if (valueTy <<: ty)
        Nil
      else
        List(UpcastException(valueTy.toString, ty.toString, getLocation))
    }
    super.validate(module) ++ validateCast
  }
}
