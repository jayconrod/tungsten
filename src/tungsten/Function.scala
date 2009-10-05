package tungsten

import Utilities._

final case class Function(override name: Symbol,
                          typeParameters: List[TypeParameter],
                          parameters: List[Parameter],
                          returnType: Type,
                          blocks: List[Block],
                          override location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    val typeParametersStr = typeParameters.mkString("[", ", ", "]")
    val parametersStr = parameters.mkString("(", ", ", ")")
    val returnTypeStr = ": " + returnType
    val bodyStr = blocks.mkString("{\n", "\n", "\n}")
    "#function " + name + typeParametersStr + parametersStr + returnTypeStr + bodyStr
  }
}
