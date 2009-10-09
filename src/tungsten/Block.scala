package tungsten

final case class Block(override name: Symbol,
                       parameters: List[Symbol],
                       instructions: List[Symbol],
                       override location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
