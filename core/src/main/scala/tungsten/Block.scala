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
    val parameterTypes = module.getParameters(parameters).map(_.ty)
    FunctionType(UnitType(location), parameterTypes)
  }

  def validateComponents(module: Module) = {
    stage(validateComponentsOfClass[Parameter](module, parameters),
          validateNonEmptyComponentsOfClass[Instruction](module, instructions))
  }

  def validate(module: Module) = {
    def checkTermination(insts: List[Instruction]): List[CompileException] = {
      insts match {
        case Nil => throw new RuntimeException("instructions must be non-empty")
        case i :: Nil => {
          if (i.isTerminating) 
            Nil
          else
            List(BlockTerminationException(name, location))
        }
        case i :: is => {
          if (!i.isTerminating) 
            checkTermination(is)
          else
            List(EarlyTerminationException(name, i.name, location))
        }
      }
    }

    checkTermination(module.getInstructions(instructions))
  }

  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
