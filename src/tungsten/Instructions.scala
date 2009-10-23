package tungsten

sealed abstract class Instruction(name: Symbol, location: Location)
  extends Definition(name, location)
  with TypedDefinition
{
  def isTerminating = false
}

final case class AssignInstruction(override name: Symbol,
                                   value: Value,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = value.ty(module)

  def validate(module: Module) = {
    value match {
      case DefinedValue(valueName, _) => {
        module.getDefn(valueName) match {
          case Some(_: Instruction) | Some(_: Parameter) => Nil
          case Some(defn) => {
            List(InappropriateSymbolException(valueName, location, defn.location, 
                                              "local variable, parameter, or literal"))
          }
          case None => List(UndefinedSymbolException(valueName, location))
        }
      }
      case _ => Nil
    }
  }
}

sealed abstract class CallInstruction(name: Symbol, arguments: List[Value], location: Location)
  extends Instruction(name, location)
{
  protected def targetName: Symbol

  protected def targetType(module: Module): FunctionType

  def ty(module: Module) = targetType(module).returnType

  def validateCall(module: Module) = {
    val parameterTypes = targetType(module).parameterTypes
    if (arguments.size != parameterTypes.size) {
      List(FunctionArgumentCountException(targetName, 
                                          arguments.size,
                                          parameterTypes.size,
                                          location))
    } else {
      (arguments zip targetType(module).parameterTypes) flatMap { at => 
        val (a, t) = at
        a.validate(module, t)
      }
    }
  }
}   

final case class BranchInstruction(override name: Symbol, 
                                   target: Symbol,
                                   arguments: List[Value],
                                   override location: Location = Nowhere)
  extends CallInstruction(name, arguments, location)
{
  override def isTerminating = true

  protected def targetName = target

  protected def targetType(module: Module) = {
    module.get[Block](target).get.ty(module)
  }

  def validate(module: Module) = {
    validateComponent[Block](module, target) ++ validateCall(module)
  }
}

final case class IndirectCallInstruction(override name: Symbol,
                                         target: Value,
                                         arguments: List[Value],
                                         override location: Location = Nowhere)
  extends CallInstruction(name, arguments, location)
{
  protected def targetName = target.asInstanceOf[DefinedValue].value

  protected def targetType(module: Module) = target.ty(module).asInstanceOf[FunctionType]

  def validate(module: Module): List[CompileException] = {
    target match {
      case DefinedValue(value, _) => {
        val errors = validateComponent[TypedDefinition](module, value)
        if (!errors.isEmpty)
          errors
        else {
          module.get[TypedDefinition](value).get.ty(module) match {
            case _: FunctionType => validateCall(module)
            case _ => List(FunctionTypeException(value.toString, location))
          }
        }
      }
      case _ => List(FunctionTypeException(target.toString, location))
    }
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
  override def isTerminating = intrinsic == Intrinsic.EXIT

  def targetName = new Symbol(intrinsic.name)

  def targetType(module: Module) = intrinsic.ty

  def validate(module: Module) = validateCall(module)    
}

final case class ReturnInstruction(override name: Symbol,
                                   value: Value,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  override def isTerminating = true

  def ty(module: Module) = UnitType(location)

  def validate(module: Module) = Nil    // return value validated in Function
}

final case class StaticCallInstruction(override name: Symbol,
                                       target: Symbol,
                                       arguments: List[Value],
                                       override location: Location = Nowhere)
  extends CallInstruction(name, arguments, location)
{
  protected def targetName = target

  protected def targetType(module: Module) = module.get[Function](target).get.ty(module)

  def validate(module: Module) = {
    validateComponent[Function](module, target) match {
      case Nil => validateCall(module)
      case errors => errors
    }
  }
}
