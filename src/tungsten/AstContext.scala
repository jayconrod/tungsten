package tungsten

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

final class AstContext {
  var module = new Module
  val errors = new ArrayBuffer[CompileException]
  val names = new Stack[Symbol]

  def addDefn(defn: Definition) = module = module.add(defn)

  def replaceDefn(defn: Definition) = module = module.replace(defn)

  def createName(name: Symbol): Symbol = {
    if (name.isSimple && !names.isEmpty)
      names.top + name
    else
      name
  }

  def resolve(name: Symbol): Option[Symbol] = {
    def nameIsDefined(fullName: Symbol): Boolean = {
      module.get(fullName).isDefined
    }

    if (nameIsDefined(name))
      Some(name)
    else if (name.isSimple)
      names.view.map(_ + name).find(nameIsDefined _)
    else
      None
  }
}
