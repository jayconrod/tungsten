package tungsten

import Utilities._

final class Interface(name: Symbol,
                      val typeParameters: List[Symbol],
                      val superclass: Option[ClassType],
                      val interfaces: List[InterfaceType],
                      val methods: List[Symbol],
                      location: Location = Nowhere)
  extends Definition(name, location)
{
  def validate(module: Module) = {
    stage(validateComponents[TypeParameter](module, typeParameters),
          superclass.toList.flatMap(_.validate(module)),
          interfaces.flatMap(_.validate(module)),
          validateComponents[Function](module, methods))
    // TODO: superclass may not implement this interface
    // TODO: superclass may not be Nothing
    // TODO: superclass must be subclass of Object
    // TODO: methods start with a supertype parameter
    // TODO: interfaces should not be defined recursively
  }

  override def toString = {
    val typeParametersStr = if (typeParameters.isEmpty) 
      ""
    else
      typeParameters.mkString("[", ", ", "]")
    val superclassStr = superclass match {
      case None => ""
      case Some(sc) => " extends " + sc
    }
    val interfacesStr = if (interfaces.isEmpty)
      ""
    else
      " implements " + interfaces.mkString(", ")
    val methodsStr = methods.mkString("methods:\n  ", "\n  ", "\n")
    "class " + name + typeParametersStr + superclassStr + interfacesStr + "\n{\n" +
      methodsStr + "}"
  }
}
