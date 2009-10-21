package tungsten

sealed abstract class Instruction(name: Symbol, location: Location)
  extends Definition(name, location)
  with TypedDefinition
{
  def isTerminating = false
}

final case class BranchInstruction(override name: Symbol, 
                                   target: Symbol,
                                   arguments: List[Value],
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  override def isTerminating = true

  def ty(module: Module) = UnitType(location)

  def validate(module: Module) = {
    validateComponent[Block](module, target)
  }
}

final case class IndirectCallInstruction(override name: Symbol,
                                         target: Value,
                                         arguments: List[Value],
                                         override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = {
    val functionTy = target.ty(module).asInstanceOf[FunctionType]
    functionTy.returnType
  }

  def validate(module: Module): List[CompileException] = {
    val name = target match {
      case DefinedValue(value, _) => value
      case _ => new Symbol("value")
    }
    val targetTy = target.ty(module)
    if (!targetTy.isInstanceOf[FunctionTypeException])
      List(FunctionTypeException(name, target.location))
    else {
      val functionTy = targetTy.asInstanceOf[FunctionType]
      var errors = if (functionTy.parameterTypes.size == arguments.size)
        Nil
      else {
        List[CompileException](FunctionArgumentCountException(name, 
                                                              arguments.size,
                                                              functionTy.parameterTypes.size,
                                                              location))
      }
      (functionTy.parameterTypes zip arguments).foldRight(errors) { (arg, errors) =>
        val (t, a) = arg
        if (t != a.ty(module)) {
          val exn = TypeMismatchException(a.ty(module).toString, t.toString, location)
          exn :: errors
        } else
          errors
      }
    }
  }
}

case class IntrinsicFunction(number: Int, 
                             name: String,
                             parameterTypes: List[Type],
                             returnType: Type)

object Intrinsic {
  val EXIT = IntrinsicFunction(1, "exit", List(IntType(32)), UnitType())
  val INTRINSICS = List(EXIT)
}

final case class IntrinsicCallInstruction(override name: Symbol,
                                          intrinsic: IntrinsicFunction,
                                          arguments: List[Value],
                                          override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = intrinsic.returnType

  def validate(module: Module) = throw new UnsupportedOperationException
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
  extends Instruction(name, location)
{
  def ty(module: Module) = {
    val function = module.get(target).get.asInstanceOf[Function]
    val functionTy = function.ty(module).asInstanceOf[FunctionType]
    functionTy.returnType
  }

  def validate(module: Module) = {
    val typeErrors = validateComponent[Function](module, target)
    if (!typeErrors.isEmpty)
      typeErrors
    else {
      val function = module.get(target).get.asInstanceOf[Function]
      val functionTy = function.ty(module).asInstanceOf[FunctionType]
      if (functionTy.parameterTypes.size != arguments.size) {
        List(FunctionArgumentCountException(target,
                                            arguments.size,
                                            functionTy.parameterTypes.size,
                                            location))
      } else {
        def check(arg: (Type, Value), errors: List[CompileException]) = {
          val (t, a) = arg
          if (t != a.ty(module)) {
            val exn = TypeMismatchException(a.ty(module).toString, t.toString, location)
            exn :: errors
          } else
            errors
        }
        (functionTy.parameterTypes zip arguments).foldRight(List[CompileException]())(check _)
      }
    }
  }
}
