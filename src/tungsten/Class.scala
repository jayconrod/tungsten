package tungsten

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
  def validate(module: Module) = {
    stage(validateComponents[TypeParameter](module, typeParameters),
          superclass.toList.flatMap(_.validate(module)),
          interfaces.flatMap(_.validate(module)),
          validateComponents[Field](module, fields),
          validateComponents[Function](module, methods))
    // TODO: superclass may not be Nothing
    // TODO: superclass must be subclass of Object
    // TODO: methods start with a supertype parameter
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
    val fieldsStr = fields.mkString("fields:\n  ", "\n  ", "\n")
    val methodsStr = methods.mkString("methods:\n  ", "\n  ", "\n")
    "class " + name + typeParametersStr + superclassStr + interfacesStr + "\n{\n" +
      fieldsStr + "\n" + methodsStr + "}"
  }
}
