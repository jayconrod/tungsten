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

  def get(name: Symbol) = definitions.get(name)

  private def definitionTypeName[T <: Definition](implicit m: Manifest[T]) = {
    val typeName = m.toString
    typeName.charAt(0).toLowerCase + typeName.tail.map({c => 
      if (c.isUpperCase) " " + c.toLowerCase else c.toString
    }).mkString
  }

  def validateName[T <: Definition](name: Symbol, location: Location)
                                   (implicit m: Manifest[T]) =
  {
    get(name) match {
      case Some(defn) if m.erasure.isInstance(defn) => None
      case Some(defn) => {
        val typeName = definitionTypeName[T]
        Some(InappropriateSymbolException(name, location, defn.location, typeName))
      }
      case None => Some(UndefinedSymbolException(name, location))
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
