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
          validateTermination)
  }

  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
