package tungsten

final case class Interface(name: Symbol,
                           typeParameters: List[Symbol],
                           supertype: ObjectType,
                           interfaceTypes: List[InterfaceType],
                           interfaceMethods: List[List[Symbol]],
                           methods: List[Symbol],
                           annotations: List[AnnotationValue] = Nil)
  extends Definition
  with ObjectDefinition
{
  override def validateComponents(module: Module): List[CompileException] = {
    super.validateComponents(module) ++
      validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      interfaceMethods.flatMap(validateComponentsOfClass[Function](module, _)) ++
      validateComponentsOfClass[Function](module, methods)
  }

  override def validate(module: Module): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }

  def getSuperType: Option[ObjectType] = Some(supertype)
}
