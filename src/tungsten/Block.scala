package tungsten

final class Block(name: Symbol,
                  val parameters: List[Symbol],
                  val instructions: List[Instruction],
                  location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    val parametersStr = parameters.mkString("(", ", ", ")")
    val instructionsStr = instructions.mkString("\n{\n  ", "\n  ", "\n}")
    name.toString + parametersStr + instructionsStr
  }
}
