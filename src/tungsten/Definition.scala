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
    val className = humanReadableClassName[T]

    def check(componentNames: List[Symbol], 
              seen: Set[Symbol],
              errors: List[CompileException]): List[CompileException] =
    {
      componentNames match {
        case Nil => errors
        case n :: ns => {
          val newErrors = if (seen.contains(n))
            DuplicateComponentException(name, n, className, location) :: errors
          else {
            module.get[T](n) match {
              case Some(defn) if m.erasure.isInstance(defn) => errors
              case Some(defn) => { 
                InappropriateSymbolException(n, 
                                             location,
                                             defn.location,
                                             className) :: errors
              }
              case None => UndefinedSymbolException(n, location) :: errors
            }
          }

          check(ns, seen + n, newErrors)
        }
      }
    }

    check(componentNames, Set(), Nil)
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
