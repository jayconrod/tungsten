package tungsten

trait ObjectDefinition 
  extends Definition
{
  def typeParameters: List[Symbol]

  final def getTypeParameters(module: Module): List[TypeParameter] = {
    module.getTypeParameters(typeParameters)
  }

  def getSuperType: Option[ObjectType]
  def interfaceTypes: List[InterfaceType]
  def interfaceMethods: List[List[Symbol]]
  def methods: List[Symbol]

  def selfType: ObjectType

  def inheritedTypes: List[ObjectType] = {
    getSuperType match {
      case Some(superType) => superType :: interfaceTypes
      case None => interfaceTypes
    }
  }

  /** Replaces type variables in the given type with the given type arguments. The
   *  list of type variables to replace comes from this definition.
   */
  def substituteInheritedType[T <: ObjectType](ty: T,
                                               typeArguments: List[Type]): T =
  {
    val substitutions = typeParameters zip typeArguments
    (ty /: substitutions) { (ty, sub) =>
      val (tyParamName, argument) = sub
      ty.substitute(tyParamName, argument).asInstanceOf[T]
    }
  }

  def getInheritedType(fromName: Symbol): ObjectType = {
    getSuperType match {
      case Some(t) if t.definitionName == fromName => t
      case _ => inheritedTypes.find(_.definitionName == fromName) match {
        case Some(t) => t
        case None => {
          System.err.println("requested type %s from %s".format(fromName, name))
          throw new IllegalArgumentException
        }
      }
    }
  }
}
