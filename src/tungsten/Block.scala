package tungsten

final case class Block(override name: Symbol,
                       parameters: List[Symbol],
                       instructions: List[Symbol],
                       override location: Location = Nowhere)
  extends Definition(name, location)
{
  def validate(module: Module) = {
    def validateComponents = {
      parameters.flatMap(validateComponent[Parameter](module, _)) ++
        instructions.flatMap(validateComponent[Instruction](module, _))
    }

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

    validateComponents ++ validateTermination
  }

  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
