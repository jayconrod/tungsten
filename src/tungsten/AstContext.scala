package tungsten

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

final class AstContext {
  val module = new Module
  val errors = new ArrayBuffer[CompileException]
  val names = new Stack[Symbol]

  def resolve(name: Symbol): Option[Symbol] = {
    for (n <- names) {
      val defn = module.get(n + name)
      if (defn.isInstanceOf[Some[_]]) 
        return defn.map(_.name)
    }
    module.get(name).map(_.name)
  }   
}
