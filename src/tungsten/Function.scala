package tungsten

import Utilities._

final class Function(name: Symbol,
                     val typeParameters: List[Symbol],
                     val parameters: List[Symbol],
                     val returnType: Type,
                     val blocks: List[Block],
                     location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    val typeParametersStr = typeParameters.mkString("[", ", ", "]")
    val parametersStr = parameters.mkString("(", ", ", ")")
    val returnTypeStr = ": " + returnType
    val bodyStr = blocks.mkString("{\n", "\n", "\n}")
    typeParametersStr + parametersStr + returnTypeStr + bodyStr
  }
}
