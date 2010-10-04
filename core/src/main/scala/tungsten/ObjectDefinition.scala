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
}
