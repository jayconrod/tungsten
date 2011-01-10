package tungsten

import Utilities._

sealed abstract class Instruction
  extends Definition
{
  def ty: Type

  def isTerminating = false

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
      val argumentCountErrors = if (arguments.size != targetType.parameterTypes.size) {
        List(FunctionArgumentCountException(targetName,
                                            arguments.size,
                                            targetType.parameterTypes.size,
                                            getLocation))
      } else
        Nil

      val substitutedTargetType = targetType.applyTypeArguments(typeArguments)
      val argumentTypes = arguments.map(_.ty)
      val argumentTypeErrors = (argumentTypes zip substitutedTargetType.parameterTypes) flatMap { p =>
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

abstract class ExtendedInstruction extends Instruction

/** A trait for instructions which deal with values within aggregate values. An example is 
 *  AddressInstruction, which calculates the address of an element inside an aggregate. All
 *  Element instructions have a base address and a number of indices. Utility methods are 
 *  provided to validate the indices and type check.
 */
trait ElementInstruction extends Instruction {
  /** Determines the type of the element being referenced by this instruction.
   *  @param baseType the type of the base value for this instruction. If the instruction
   *    deals with pointers, this is the base type of the pointer. If the instruction
   *    deals directly with aggregates, this is the type of the aggregate.
   *  @param indices a list of index values. These have 32- or 64-bit integer type depending
   *    on whether the module is 64-bit.
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

  /** Like getElementType, but intended for instructions that deal with pointers to aggregates.
   *  The first index is treated like an array index, and the rest are treated normally.
   *  @param pointerType the type of the base pointer value. This must be a non-null pointer.
   *    Call validatePointerIndices first if you aren't sure.
   *  @param indices the indices to the element. These should be validated first by 
   *    validatePointerIndices.
   *  @return a pointer type to the element being referenced by the indices
   */
  def getPointerType(module: Module, pointerType: Type, indices: List[Value]): PointerType = 
  {
    (pointerType, indices) match {
      case (PointerType(baseType), i :: is) => {
        val elementType = getElementType(module, baseType, is)
        PointerType(elementType)
      }
      case (PointerType(_), Nil) => throw new RuntimeException("invalid indices; did you call validatePointerIndices first")
      case _ => throw new RuntimeException("invalidate pointer type; did you call validatePointerIndices first?")
    }
  }

  /** Like validateIndicies, but intended for instructions that deal with pointers to 
   *  aggregates. The first index in these instructions is treated like an array index. 
   *  The remaining indices are treated normally.
   *  @param baseType the type of the base pointer value. This will usually be a pointer type.
   *    an error will be returned if it is not.
   */
  def validatePointerIndices(module: Module, 
                             pointerType: Type,
                             indices: List[Value]): List[CompileException] =
  {
    (pointerType, indices) match {
      case (PointerType(baseType), i :: is) => {
        checkType(i.ty, IntType.wordType(module), getLocation) ++ 
          validateIndices(module, baseType, is)
      }
      case (PointerType(_), Nil) => List(MissingElementIndexException(getLocation))
      case _ => List(TypeMismatchException(pointerType.toString, "pointer type", getLocation))
    }
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
        List(UnsupportedNumericOperationException(lty, operator, getLocation))
      else if (lty != rty)
        List(TypeMismatchException(rty.toString, lty.toString, getLocation))
      else
        Nil
    }

    stage(super.validate(module) ++ validateOperation,
          checkType(left.ty, ty, getLocation))
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

  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def liveOutBindings: Map[Symbol, List[Value]] = Map(target -> arguments)

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentOfClass[Block](module, target)
  }

  override def validate(module: Module) = {
    val block = module.getBlock(target)
    val parameters = module.getParameters(block.parameters)
    val parameterTypes = parameters.map(_.ty)
    super.validate(module) ++ 
      validateCall(module, target, block.ty(module), Nil, arguments, ty)
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

  def operands = condition :: trueArguments ++ falseArguments

  override def usedSymbols = trueTarget :: falseTarget :: operandSymbols

  override def liveOutBindings: Map[Symbol, List[Value]] = {
    Map(trueTarget  -> trueArguments,
        falseTarget -> falseArguments)
  }

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

case class IntrinsicFunction(number: Int, 
                             name: String,
                             ty: FunctionType)

object Intrinsic {
  val EXIT = IntrinsicFunction(1, "exit", FunctionType(UnitType, Nil, List(IntType(32))))
  val INTRINSICS = List(EXIT)
}

final case class IntrinsicCallInstruction(name: Symbol,
                                          ty: Type,
                                          intrinsic: IntrinsicFunction,
                                          arguments: List[Value],
                                          annotations: List[AnnotationValue] = Nil)
  extends Instruction with CallInstruction
{
  override def isTerminating = intrinsic == Intrinsic.EXIT

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
      val PointerType(elementType) = pointer.ty
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
  extends Instruction with ElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: indices

  override def validate(module: Module) = {
    def validateType = {
      val PointerType(elementType) = getPointerType(module, base.ty, indices)
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
    val ctorType = module.getFunction(constructorName).ty(module)
    val newType = ctorType.copy(returnType = classType)
    val clas = classType.getDefinition(module)
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
        List(UnsupportedNumericOperationException(lty, operator, getLocation))
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
  with ElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: value :: indices

  override def validate(module: Module) = {
    def validateType = {
      val PointerType(elementType) = getPointerType(module, base.ty, indices)
      checkType(value.ty, elementType, getLocation)
    }

    super.validate(module) ++
      stage(validatePointerIndices(module, base.ty, indices),
            validateType,
            checkType(UnitType, ty, getLocation))
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
