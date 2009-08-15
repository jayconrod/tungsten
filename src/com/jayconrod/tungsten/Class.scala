package com.jayconrod.tungsten

import Utilities._

final class Class(name: Symbol,
                  val typeParameters: List[Symbol],
                  val superclass: Option[ClassType],
                  val interfaces: List[InterfaceType],
                  val fields: List[Symbol],
                  val methods: List[Symbol],
                  location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    val typeParametersStr = if (typeParameters.isEmpty) 
      ""
    else
      "[" + joinStrings(", ", typeParameters) + "]"
    val superclassStr = superclass match {
      case None => ""
      case Some(sc) => " extends " + sc
    }
    val interfacesStr = if (interfaces.isEmpty)
      ""
    else
      " implements " + joinStrings(", ", interfaces)
    val fieldsStr = "fields:\n  " + joinStrings("\n  ", fields) + "\n"
    val methodsStr = "methods:\n  " + joinStrings("\n  ", methods) + "\n"
    "class " + name + typeParametersStr + superclassStr + interfacesStr + "\n{\n" +
      fieldsStr + "\n" + methodsStr + "}"
  }
}
