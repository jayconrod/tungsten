package tungsten

abstract class Definition(val name: Symbol, location: Location = Nowhere) 
  extends TungstenObject(location)
{
  def validate(module: Module): List[CompileException]

  protected final def validateComponent[T <: Definition](module: Module,
                                                         componentName: Symbol)
                                                        (implicit m: Manifest[T]) =
  {
    module.getDefn(componentName) match {
      case Some(defn) => defn.validateType[T](module, location)
      case None => List(UndefinedSymbolException(componentName, location))
    }
  }

  final def validateType[T <: Definition](module: Module,
                                          parentLoc: Location)
                                         (implicit m: Manifest[T]) =
  {
    val typeName = m.toString.charAt(0).toLowerCase + m.toString.tail.map({c => 
      if (c.isUpperCase) " " + c.toLowerCase else c.toString
    }).mkString

    if (m.erasure.isInstance(this))
      Nil
    else
      List(InappropriateSymbolException(name, parentLoc, location, typeName))
  }

  def checkName(module: Module) = {
    assert(module.get(name) eq this)
  }

  def toString: String
}

trait TypedDefinition extends Definition {
  def ty(module: Module): Type
}
