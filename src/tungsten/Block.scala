package tungsten

import Utilities._

final case class Block(override name: Symbol,
                       parameters: List[Symbol],
                       instructions: List[Symbol],
                       override location: Location = Nowhere)
  extends Definition(name, location)
  with TypedDefinition
{
  def ty(module: Module): FunctionType = {
    val parameterTypes = parameters.map(module.get[Parameter](_).get.ty)
    FunctionType(UnitType(location), parameterTypes)
  }

  def validate(module: Module) = {
    def validateTermination = {
      if (instructions.isEmpty)
        List(EmptyBlockException(name, location))
      else {
        module.get[Instruction](instructions.last) match {
          case Some(inst) if inst.isTerminating => Nil
          case _ => List(BlockTerminationException(name, location))
        }
      }
    }

    stage(validateComponents[Parameter](module, parameters),
          validateComponents[Instruction](module, instructions),
          validateTermination)
  }

  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
