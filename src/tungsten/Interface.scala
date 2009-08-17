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
