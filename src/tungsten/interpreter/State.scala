package tungsten.interpreter

import scala.collection.mutable.HashMap
import tungsten.Instruction
import tungsten.Symbol

final class State(var ip: Iterable[Instruction]) {
  val values = new HashMap[Symbol, Value]

  override def toString = {
    "ip @ " + ip.head.name + values.mkString("\n", "\n", "\n")
  }
}
