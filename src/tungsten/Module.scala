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

  def getDefn(name: Symbol): Option[Definition] = definitions.get(name)

  def get[T <: Definition](name: Symbol)(implicit m: Manifest[T]): Option[T] = {
    definitions.get(name) match {
      case Some(d) if m.erasure.isInstance(d) => Some(d.asInstanceOf[T])
      case _ => None
    }
  }

  def validate = {
    val errors = definitions.valueIterable.flatMap(_.validate(this)).toList
    definitions.get(new Symbol("main")) match {
      case Some(_: Function) => errors
      case _ => MissingMainException() :: errors
    }
  }

  def validateName[T <: Definition](name: Symbol,
                                    location: Location)
                                   (implicit m: Manifest[T]): List[CompileException] =
  {
    getDefn(name) match {
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
