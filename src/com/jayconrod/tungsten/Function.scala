package com.jayconrod.tungsten

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
    val typeParametersStr = "[" + joinStrings(", ", typeParameters.map(_.toString)) + "]"
    val parametersStr = "(" + joinStrings(", ", parameters.map(_.toString)) + ")"
    val returnTypeStr = ": " + returnType
    val bodyStr = " {\n" + joinStrings("\n", blocks.map(_.toString)) + "\n}"
    typeParametersStr + parametersStr + returnTypeStr + bodyStr
  }
}
