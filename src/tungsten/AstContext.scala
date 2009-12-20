package tungsten

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

final class AstContext {
  var module = new Module
  val errors = new ArrayBuffer[CompileException]
  val names = new Stack[Symbol]

  def addDefn(defn: Definition) = module = module.add(defn)

  def replaceDefn(defn: Definition) = module = module.replace(defn)

  def resolve(name: Symbol): Option[Symbol] = {
    for (n <- names) {
      val fullName = n + name
      if (module.get(fullName).isDefined)
        return Some(fullName)
    }
    if (module.get(name).isDefined)
      Some(name)
    else
      None
  }   
}
