package tungsten

import Utilities._

final class Block(name: Symbol,
                  val parameters: List[Symbol],
                  val instructions: List[Instruction],
                  location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    name + "(" + joinStrings(", ", parameters.map(_.toString)) + ")\n{\n  " +
      joinStrings("\n  ", instructions.map(_.toString)) + "\n}"
  }
}
