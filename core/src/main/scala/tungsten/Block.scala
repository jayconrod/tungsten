package tungsten

import Utilities._

final case class Block(override name: Symbol,
                       parameters: List[Symbol],
                       instructions: List[Symbol],
                       override annotations: List[AnnotationValue] = Nil,
                       override location: Location = Nowhere)
  extends Definition(name, annotations, location)
  with TypedDefinition
{
  def ty(module: Module): FunctionType = {
    val parameterTypes = module.getParameters(parameters).map(_.ty)
    FunctionType(UnitType(location), parameterTypes)
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

    super.validate(module) ++ 
      checkTermination(module.getInstructions(instructions))
  }
}
