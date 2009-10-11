package tungsten

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.reflect.Manifest
import Utilities._

final class Module {
  private val definitions: Map[Symbol, Definition] = new HashMap[Symbol, Definition]

  def add(defn: Definition) = {
    definitions.get(defn.name) match {
      case Some(d) => throw new RedefinedSymbolException(defn.name, defn.location, d.location)
      case None => definitions += defn.name -> defn
    }
  }

  def get(name: Symbol): Option[Definition] = definitions.get(name)

  def validateName[T <: Definition](name: Symbol,
                                    location: Location)
                                   (implicit m: Manifest[T]): List[CompileException] =
  {
    get(name) match {
      case Some(defn) => defn.validateType[T](this, location)
      case None => List(UndefinedSymbolException(name, location))
    }
  }

  override def equals(that: Any) = that match {
    case m: Module => {
      definitions equals m.definitions
    }
    case _ => false
  }

  override def hashCode = definitions.hashCode
}
