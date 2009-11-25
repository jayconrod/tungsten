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
    def validateComponents[T <: Definition](implicit m: Manifest[T]) = {
      val components = _definitions.valueIterable.filter(m.erasure.isInstance(_))
      components.flatMap(_.validate(this)).toList
    }

    def validateMain = {
      _definitions.get(new Symbol("main")) match {
        case Some(main: Function) => {
          if (!main.parameters.isEmpty)
            List(MainNonEmptyParametersException(main.location))
          else if (main.returnType != UnitType())
            List(MainReturnTypeException(main.location))
          else
            Nil
        }
        case _ => List(MissingMainException())
      }
    }

    stage(validateComponents[Function],
          validateComponents[Class],
          validateComponents[Interface],
          validateComponents[Global],
          validateComponents[Parameter],
          validateComponents[TypeParameter],
          validateComponents[Struct],
          validateComponents[Block],
          validateComponents[Instruction],
          validateMain)
  }

  override def equals(that: Any) = that match {
    case m: Module => {
      _definitions equals m._definitions
    }
    case _ => false
  }

  override def hashCode = _definitions.hashCode

  override def toString = _definitions.toString
}
