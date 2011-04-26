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

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    def validateInstructions(scope: Set[Symbol], 
                             instructions: List[Symbol],
                             errors: List[CompileException]): List[CompileException] =
    {
      instructions match {
        case Nil => errors
        case h :: t => {
          val instruction = module.getInstruction(h)
          val newErrors = instruction.validateScope(module, scope) ++ errors
          val newScope = scope + h
          validateInstructions(newScope, t, newErrors)
        }
      }
    }

    validateComponentsScope(module, scope, parameters) ++
      validateInstructions(scope ++ parameters, instructions, Nil)
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
