package tungsten

sealed abstract class Instruction(name: Symbol, location: Location)
  extends Definition(name, location)
  with TypedDefinition

final case class BranchInstruction(override name: Symbol, 
                                   target: Symbol,
                                   arguments: List[Value],
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = UnitType(location)

  def validate(module: Module) = {
    validateComponent[Block](module, target) ++ arguments.flatMap(_.validate(module))
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

final case class ReturnInstruction(override name: Symbol,
                                   value: Value,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = UnitType(location)

  def validate(module: Module) = {
    value.validate(module)
  }
}

