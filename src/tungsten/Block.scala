package tungsten

final case class Block(override name: Symbol,
                       parameters: List[Parameter],
                       instructions: List[Instruction],
                       override location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
