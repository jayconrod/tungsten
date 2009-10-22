package tungsten

import Utilities._

abstract class Definition(val name: Symbol, location: Location = Nowhere) 
  extends TungstenObject(location)
{
  def validate(module: Module): List[CompileException]

  protected def validateComponents[T <: Definition](module: Module,
                                                    componentNames: List[Symbol])
                                                   (implicit m: Manifest[T]) =
  {
    componentNames flatMap { n =>
      module.get[T](n) match {
        case Some(defn) if m.erasure.isInstance(defn) => Nil
        case Some(defn) => { 
          List(InappropriateSymbolException(n, 
                                            location, 
                                            defn.location,
                                            humanReadableClassName[T]))
        }
        case None => List(UndefinedSymbolException(n, location))
      }
    }
  }

  protected def validateComponent[T <: Definition](module: Module,
                                                   componentName: Symbol)
                                                  (implicit m: Manifest[T]) =
  {
    validateComponents[T](module, List(componentName))
  }

  def toString: String
}

trait TypedDefinition extends Definition {
  def ty(module: Module): Type
}
