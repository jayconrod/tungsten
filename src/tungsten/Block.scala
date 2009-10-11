package tungsten

final case class Block(override name: Symbol,
                       parameters: List[Symbol],
                       instructions: List[Symbol],
                       override location: Location = Nowhere)
  extends Definition(name, location)
{
  def validate(module: Module) = {
    parameters.flatMap(validateComponent[Parameter](module, _)) ++
      instructions.flatMap(validateComponent[Instruction](module, _))
  }

  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
