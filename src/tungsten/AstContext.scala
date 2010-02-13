package tungsten

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.io.File
import Utilities._

final class AstContext(name: Symbol,
                       ty: ModuleType,
                       version: Version,
                       filename: Option[File],
                       dependencies: List[ModuleDependency],
                       searchPaths: List[File])
{
  def this() = {
    this("default",
         ModuleType.INTERMEDIATE,
         Version.MIN,
         None,
         Nil,
         Nil)
  }

  var module = new Module(name, ty, version, 
                          filename, dependencies, searchPaths, 
                          Map[Symbol, Definition]())
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
