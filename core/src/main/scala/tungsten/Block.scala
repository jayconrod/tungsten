package tungsten

import Utilities._

final case class Block(name: Symbol,
                       parameters: List[Symbol],
                       instructions: List[Symbol],
                       annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  def ty(module: Module): FunctionType = {
    val parameterTypes = module.getParameters(parameters).map(_.ty)
    FunctionType(UnitType, Nil, parameterTypes)
  }

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Parameter](module, parameters) ++
      validateNonEmptyComponentsOfClass[Instruction](module, instructions)
  }

  override def validate(module: Module) = {
    def checkTermination(insts: List[Instruction]): List[CompileException] = {
      insts match {
        case Nil => throw new RuntimeException("instructions must be non-empty")
        case i :: Nil => {
          if (i.isTerminating) 
            Nil
          else
            List(BlockTerminationException(name, getLocation))
        }
        case i :: is => {
          if (!i.isTerminating) 
            checkTermination(is)
          else
            List(EarlyTerminationException(name, i.name, getLocation))
        }
      }
    }

    super.validate(module) ++ 
      checkTermination(module.getInstructions(instructions))
  }
}
