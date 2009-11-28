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

sealed abstract class CallInstruction(name: Symbol, arguments: List[Value], location: Location)
  extends Instruction(name, location)
{
  protected def targetName: Symbol

  protected def targetType(module: Module): FunctionType

  def ty(module: Module) = targetType(module).returnType

  override def validate(module: Module) = { 
    validateCall(module, targetType(module).parameterTypes)
  }

  protected def validateCall(module: Module, 
                             parameterTypes: List[Type]): List[CompileException] = 
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
  val REMAINDER = BinaryOperator("/")
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
  extends CallInstruction(name, arguments, location)
{
  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  override def isTerminating = true

  protected def targetName = throw new UnsupportedOperationException

  protected def targetType(module: Module) = {
    module.getBlock(target).ty(module)
  }

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          validateComponentOfClass[Block](module, target))
  }
}

final case class ConditionalBranchInstruction(override name: Symbol,
                                              condition: Value,
                                              trueTarget: Symbol,
                                              falseTarget: Symbol,
                                              arguments: List[Value],
                                              override location: Location = Nowhere)
  extends CallInstruction(name, arguments, location)
{
  def operands = condition :: arguments

  override def usedSymbols = trueTarget :: falseTarget :: operandSymbols

  override def isTerminating = true

  protected def targetName = trueTarget

  protected def targetType(module: Module) = {
    module.getBlock(trueTarget).ty(module)
  }

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          validateComponentsOfClass[Block](module, List(trueTarget, falseTarget)))
  }

  override def validate(module: Module) = {
    // We assume that both of the branch targets refer to local blocks and that the block
    // parameters are validated. This should be done by Block and Function validation

    def validateBranch(target: Symbol) = {
      val block = module.getBlock(target)
      val parameterTypes = module.getParameters(block.parameters).map(_.ty)
      validateCall(module, parameterTypes)
    }

    stage(validateBranch(trueTarget),
          validateBranch(falseTarget),
          condition.validateType(module, BooleanType()))
  }
}

final case class GlobalLoadInstruction(override name: Symbol,
                                       globalName: Symbol,
                                       override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def operands = Nil

  override def usedSymbols = List(globalName)

  def ty(module: Module) = {
    module.getGlobal(globalName).ty
  }

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
  def operands = List(value)

  override def usedSymbols = globalName :: operandSymbols

  def ty(module: Module) = UnitType()

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
  extends CallInstruction(name, arguments, location)
{
  def operands = target :: arguments

  protected def targetName = target.asInstanceOf[DefinedValue].value

  protected def targetType(module: Module) = target.ty(module).asInstanceOf[FunctionType]

  override def validate(module: Module): List[CompileException] = {
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
  extends CallInstruction(name, arguments, location)
{
  def operands = arguments

  override def isTerminating = intrinsic == Intrinsic.EXIT

  def targetName = new Symbol(intrinsic.name)

  def targetType(module: Module) = intrinsic.ty
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
  def operands = Nil

  def ty(module: Module) = ty

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

final case class StaticCallInstruction(override name: Symbol,
                                       target: Symbol,
                                       arguments: List[Value],
                                       override location: Location = Nowhere)
  extends CallInstruction(name, arguments, location)
{
  def operands = arguments

  override def usedSymbols = target :: operandSymbols

  protected def targetName = target

  protected def targetType(module: Module) = module.getFunction(target).ty(module)

  override def validateComponents(module: Module) = {
    stage(super.validateComponents(module),
          validateComponentOfClass[Function](module, target))
  }
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
    if (!valueTy.isPointer || !ty.isPointer || !(valueTy <<: ty))
      List(UpcastException(valueTy.toString, ty.toString, location))
    else
      Nil
  }
}
