package tungsten

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.reflect.Manifest
import Utilities._

final class Module {
  private val _definitions: Map[Symbol, Definition] = new HashMap[Symbol, Definition]

  def definitions = _definitions

  def add(defn: Definition) = {
    _definitions.get(defn.name) match {
      case Some(d) => throw new RedefinedSymbolException(defn.name, defn.location, d.location)
      case None => _definitions += defn.name -> defn
    }
    ()
  }

  def update(defn: Definition) = {
    _definitions += defn.name -> defn
  }

  def getDefn(name: Symbol): Option[Definition] = _definitions.get(name)

  def get[T <: Definition](name: Symbol)(implicit m: Manifest[T]): Option[T] = {
    _definitions.get(name) match {
      case Some(d) if m.erasure.isInstance(d) => Some(d.asInstanceOf[T])
      case _ => None
    }
  }

  def validate = {
    val errors = _definitions.valueIterable.flatMap(_.validate(this)).toList
    _definitions.get(new Symbol("main")) match {
      case Some(_: Function) => errors
      case _ => MissingMainException() :: errors
    }
  }

  def validateName[T <: Definition](name: Symbol,
                                    location: Location)
                                   (implicit m: Manifest[T]): List[CompileException] =
  {
    getDefn(name) match {
      case Some(defn) if m.erasure.isInstance(name) => Nil
      case Some(defn) => {
        List(InappropriateSymbolException(name, 
                                          location, 
                                          defn.location, 
                                          humanReadableClassName[T]))
      }
      case None => List(UndefinedSymbolException(name, location))
    }
  }

  override def equals(that: Any) = that match {
    case m: Module => {
      _definitions equals m._definitions
    }
    case _ => false
  }

  override def hashCode = _definitions.hashCode
}
