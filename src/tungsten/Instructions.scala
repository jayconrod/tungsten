package tungsten

import Utilities._

sealed abstract class Instruction(name: Symbol, location: Location)
  extends Definition(name, location)
  with TypedDefinition
{
  def isTerminating = false

  def operands: List[Value]

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

  def validateComponents(module: Module): List[CompileException] = validateOperands(module)

  def validate(module: Module): List[CompileException] = Nil

  protected final def validateOperands(module: Module) = {
    operandSymbols flatMap { sym =>
      module.getDefn(sym) match {
        case Some(_: Parameter) | Some(_: Instruction) => Nil
        case Some(defn) => {
          List(InappropriateSymbolException(sym,
                                            location,
                                            defn.location,
                                            "parameter or instruction"))
        }
        case None => List(UndefinedSymbolException(sym, location))
      }
    }
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
                                          location))
    } else {
      (arguments zip parameterTypes) flatMap { at => 
        val (a, t) = at
        a.validateType(module, t)
      }
    }
  }
}   

sealed trait ElementInstruction extends Instruction {
  protected final def getElementType(baseType: Type, indices: List[Value]): Type = {
    indices match {
      case Nil => baseType
      case i :: is => {
        baseType match {
          case ArrayType(_, elementType, _) => getElementType(elementType, is)
          case _ => baseType  // bogus, but we'll catch it in validation
        }
      }
    }
  }       

  protected final def validateIndices(module: Module,
                                      baseType: Type, 
                                      indices: List[Value]): List[CompileException] =
  {
    def check(baseType: Type, 
              indices: List[Value],
              errors: List[CompileException]): List[CompileException] =
    {
      var newErrors = errors
      indices match {
        case Nil => newErrors
        case i :: is => {
          if (i.ty(module) != IntType(64)) {
            val exn = InvalidIndexException(i.toString, baseType.toString, location)
            newErrors = exn :: newErrors
          }

          baseType match {
            case ArrayType(size, elementType, _) => 
              check(elementType, is, newErrors)
            case _ => InvalidIndexException(i.toString, baseType.toString, location) :: newErrors
          }
        }
      }
    }
    check(baseType, indices, Nil)
  }
}

final case class AddressInstruction(override name: Symbol,
                                    base: Value,
                                    indices: List[Value],
                                    override location: Location = Nowhere)
  extends Instruction(name, location) with ElementInstruction
{
  def ty(module: Module) = {
    base.ty(module) match {
      case PointerType(elementType, _) => {
        val resultElementType = getElementType(elementType, indices)
        PointerType(resultElementType, location)
      }
      case _ => PointerType(UnitType(), location) // bogus, but we catch it in validation
    }
  }

  def operands = base :: indices

  override def validate(module: Module) = {
    val baseType = base.ty(module)
    def baseIsPointer = {
      if (baseType.isInstanceOf[PointerType])
        Nil
      else
        List(TypeMismatchException(baseType.toString, "non-null pointer type", location))
    }

    def indicesAreValid = {
      val elementType = baseType.asInstanceOf[PointerType].elementType
      validateIndices(module, elementType, indices)
    }

    stage(baseIsPointer,
          indicesAreValid)
  }
}

final case class AssignInstruction(override name: Symbol,
                                   value: Value,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = List(value)

  def ty(module: Module) = value.ty(module)
}

final case class BinaryOperator(name: String)

object BinaryOperator {
  val MULTIPLY = BinaryOperator("*")
  val DIVIDE = BinaryOperator("/")
  val REMAINDER = BinaryOperator("%")
  val ADD = BinaryOperator("+")
  val SUBTRACT = BinaryOperator("-")
  val LEFT_SHIFT = BinaryOperator("<<")
  val RIGHT_SHIFT_ARITHMETIC = BinaryOperator(">>")
  val RIGHT_SHIFT_LOGICAL = BinaryOperator(">>>")
  val AND = BinaryOperator("&")
  val XOR = BinaryOperator("^")
  val OR = BinaryOperator("|")

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

  def isBitOperation(op: BinaryOperator) = {
    op == LEFT_SHIFT || op == RIGHT_SHIFT_ARITHMETIC || op == RIGHT_SHIFT_LOGICAL ||
      op == AND || op == XOR || op == OR
  }
}

final case class BinaryOperatorInstruction(override name: Symbol,
                                           operator: BinaryOperator,
                                           left: Value,
                                           right: Value,
                                           override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = List(left, right)

  def ty(module: Module) = left.ty(module)

  override def validate(module: Module) = {
    def validateType = {
      val lty = left.ty(module)
      val rty = right.ty(module)
      if (!lty.isNumeric)
        List(TypeMismatchException(lty.toString, "numeric type", left.location))
      else if (lty != rty)
        List(TypeMismatchException(rty.toString, lty.toString, right.location))
      else
        Nil
    }

    def validateFloatBitOp = {
      val lty = left.ty(module)
      if (lty.isInstanceOf[FloatType] && BinaryOperator.isBitOperation(operator))
        List(FloatBitOperationException(location))
      else
        Nil
    }

    stage(validateType,
          validateFloatBitOp)
  }
}

final case class BranchInstruction(override name: Symbol, 
                                   target: Symbol,
                                   arguments: List[Value],
                                   override location: Location = Nowhere)
  extends Instruction(name, location) with CallInstruction
{
  override def isTerminating = true

  def ty(module: Module) = UnitType(location)

  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          validateComponentOfClass[Block](module, target))
  }

  override def validate(module: Module) = {
    val block = module.getBlock(target)
    val parameters = module.getParameters(block.parameters)
    val parameterTypes = parameters.map(_.ty)
    validateCall(module, target, parameterTypes, arguments)
  }
}

final case class ConditionalBranchInstruction(override name: Symbol,
                                              condition: Value,
                                              trueTarget: Symbol,
                                              trueArguments: List[Value],
                                              falseTarget: Symbol,
                                              falseArguments: List[Value],
                                              override location: Location = Nowhere)
  extends Instruction(name, location) with CallInstruction
{
  def ty(module: Module) = UnitType(location)

  override def isTerminating = true

  def operands = condition :: trueArguments ++ falseArguments

  override def usedSymbols = trueTarget :: falseTarget :: operandSymbols

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          validateComponentsOfClass[Block](module, List(trueTarget, falseTarget)))
  }

  override def validate(module: Module) = {
    // We assume that both of the branch targets refer to local blocks and that the block
    // parameters are validated. This should be done by Block and Function validation

    def validateBranch(target: Symbol, arguments: List[Value]) = {
      val block = module.getBlock(target)
      val parameterTypes = module.getParameters(block.parameters).map(_.ty)
      validateCall(module, target, parameterTypes, arguments)
    }

    stage(validateBranch(trueTarget, trueArguments),
          validateBranch(falseTarget, falseArguments),
          condition.validateType(module, BooleanType()))
  }
}

final case class GlobalLoadInstruction(override name: Symbol,
                                       globalName: Symbol,
                                       override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = {
    module.getGlobal(globalName).ty
  }

  def operands = Nil

  override def usedSymbols = List(globalName)

  override def validateComponents(module: Module) = {
    validateComponentOfClass[Global](module, globalName)
  }
}

final case class GlobalStoreInstruction(override name: Symbol,
                                        globalName: Symbol,
                                        value: Value,
                                        override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = UnitType()

  def operands = List(value)

  override def usedSymbols = globalName :: operandSymbols

  override def validateComponents(module: Module) = {
    validateComponentOfClass[Global](module, globalName)
  }

  override def validate(module: Module) = {
    value.validateType(module, module.getGlobal(globalName).ty)
  }
}

final case class IndirectCallInstruction(override name: Symbol,
                                         target: Value,
                                         arguments: List[Value],
                                         override location: Location = Nowhere)
  extends Instruction(name, location) with CallInstruction
{
  def ty(module: Module) = targetType(module).returnType

  def operands = target :: arguments

  private def targetName = target.asInstanceOf[DefinedValue].value

  private def targetType(module: Module) = target.ty(module).asInstanceOf[FunctionType]

  override def validate(module: Module): List[CompileException] = {
    // TODO: implement or delete
    throw new UnsupportedOperationException
  }
}

case class IntrinsicFunction(number: Int, 
                             name: String,
                             ty: FunctionType)

object Intrinsic {
  val EXIT = IntrinsicFunction(1, "exit", FunctionType(UnitType(), List(IntType(32))))
  val INTRINSICS = List(EXIT)
}

final case class IntrinsicCallInstruction(override name: Symbol,
                                          intrinsic: IntrinsicFunction,
                                          arguments: List[Value],
                                          override location: Location = Nowhere)
  extends Instruction(name, location) with CallInstruction
{
  def ty(module: Module) = intrinsic.ty.returnType

  override def isTerminating = intrinsic == Intrinsic.EXIT

  def operands = arguments

  override def validate(module: Module) = {
    validateCall(module, new Symbol(intrinsic.name), intrinsic.ty.parameterTypes, arguments)
  }
}

final case class LoadInstruction(override name: Symbol,
                                 pointer: Value,
                                 override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = List(pointer)

  def ty(module: Module) = {
    val pointerType = pointer.ty(module).asInstanceOf[PointerType]
    pointerType.elementType
  }
  
  override def validate(module: Module) = {
    val pointerType = pointer.ty(module)
    if (!pointerType.isInstanceOf[PointerType])
      List(TypeMismatchException(pointerType.toString, "non-null pointer type", location))
    else
      Nil
  }
}

final case class LoadElementInstruction(override name: Symbol,
                                        base: Value,
                                        indices: List[Value],
                                        override location: Location = Nowhere)
  extends Instruction(name, location) with ElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: indices

  def ty(module: Module) = getElementType(base.ty(module), indices)

  override def validate(module: Module) = validateIndices(module, base.ty(module), indices)
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

final case class RelationalOperatorInstruction(override name: Symbol,
                                               operator: RelationalOperator,
                                               left: Value,
                                               right: Value,
                                               override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = List(left, right)

  def ty(module: Module) = BooleanType(location)

  override def validate(module: Module) = {
    import RelationalOperator._
    val lty = left.ty(module)
    val rty = right.ty(module)
    if ((operator == LESS_THAN ||
         operator == LESS_EQUAL ||
         operator == GREATER_THAN ||
         operator == GREATER_EQUAL) &&
        !lty.isNumeric)
    {
      List(TypeMismatchException(lty.toString, "numeric type", left.location))
    } else if (lty != rty)
      List(TypeMismatchException(rty.toString, lty.toString, right.location))
    else
      Nil
  }
}

final case class StoreInstruction(override name: Symbol,
                                  pointer: Value,
                                  value: Value,
                                  override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = List(pointer, value)

  def ty(module: Module) = UnitType(location)

  override def validate(module: Module) = {
    val pointerType = pointer.ty(module)
    pointerType match {
      case PointerType(elementType, _) => {
        val valueType = value.ty(module)
        if (valueType != elementType)
          List(TypeMismatchException(valueType.toString, elementType.toString, location))
        else
          Nil
      }
      case _ => 
        List(TypeMismatchException(pointerType.toString, "non-null pointer type", location))
    }
  }
}

final case class StoreElementInstruction(override name: Symbol,
                                         base: Value,
                                         indices: List[Value],
                                         value: Value,
                                         override location: Location = Nowhere)
  extends Instruction(name, location) with ElementInstruction
{
  if (indices.isEmpty)
    throw new IllegalArgumentException

  def operands = base :: value :: indices

  def ty(module: Module) = UnitType(location)

  override def validate(module: Module) = {
    def validateValueType = {
      val elementType = getElementType(base.ty(module), indices)
      value.validateType(module, elementType)
    }
    stage(validateIndices(module, base.ty(module), indices),
          validateValueType)
  }
}

final case class ReturnInstruction(override name: Symbol,
                                   value: Value,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = List(value)

  override def isTerminating = true

  def ty(module: Module) = UnitType(location)
}

final case class StackAllocateInstruction(override name: Symbol,
                                          ty: Type,
                                          override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = ty

  def operands = Nil

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          ty.validate(module))
  }

  override def validate(module: Module) = {
    if (!ty.isPointer || ty.isInstanceOf[NullType])
      List(TypeMismatchException(ty.toString, "non-null pointer type", location))
    else
      Nil
  }
}

final case class StackAllocateArrayInstruction(override name: Symbol,
                                               count: Value,
                                               elementType: Type,
                                               override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = PointerType(ArrayType(None, elementType, location), location)

  def operands = List(count)

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          elementType.validate(module))
  }

  override def validate(module: Module) = {
    count.validateType(module, IntType(64))
  }
}

final case class StaticCallInstruction(override name: Symbol,
                                       target: Symbol,
                                       arguments: List[Value],
                                       override location: Location = Nowhere)
  extends Instruction(name, location) with CallInstruction
{
  def ty(module: Module) = targetType(module).returnType

  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          validateComponentOfClass[Function](module, target))
  }

  override def validate(module: Module) = {
    validateCall(module, target, targetType(module).parameterTypes, arguments)
  }

  private def targetName = target

  private def targetType(module: Module) = module.getFunction(target).ty(module)
}

final case class UpcastInstruction(override name: Symbol,
                                   value: Value,
                                   ty: Type,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = List(value)

  def ty(module: Module) = ty

  override def validate(module: Module) = {
    val valueTy = value.ty(module)
    if (valueTy <<: ty)
      Nil
    else
      List(UpcastException(valueTy.toString, ty.toString, location))
  }
}
