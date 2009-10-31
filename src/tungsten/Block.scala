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
    def validateInstructionOrder = {
      def checkOrder(insts: List[Instruction], 
                     names: Set[Symbol],
                     errors: List[CompileException]): List[CompileException] =
      {
        insts match {
          case Nil => errors
          case i :: is => {
            val invalidNames = i.operandSymbols.filter(!names.contains(_))
            val newErrors = invalidNames.map(InstructionOrderException(_, i.location))
            checkOrder(is, names + i.name, newErrors ++ errors)
          }
        }
      }
      val names = parameters.foldLeft(Set[Symbol]())(_ + _)
      val insts = instructions.map(module.get[Instruction](_).get)
      checkOrder(insts, names, Nil)
    }

    def validateTermination = {
      def check(insts: List[Instruction]): List[CompileException] = {
        insts match {
          case Nil => List(EmptyBlockException(name, location))
          case i :: Nil => {
            if (i.isTerminating) 
              Nil
            else
              List(BlockTerminationException(name, location))
          }
          case i :: is => {
            if (!i.isTerminating) 
              check(is)
            else
              List(EarlyTerminationException(name, i.name, location))
          }
        }
      }

      check(instructions.map(module.get[Instruction](_).get))
    }

    stage(validateComponents[Parameter](module, parameters),
          validateComponents[Instruction](module, instructions),
          validateInstructionOrder,
          validateTermination)
  }

  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
