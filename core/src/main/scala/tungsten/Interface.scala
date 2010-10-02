package tungsten

final case class Interface(name: Symbol,
                           typeParameters: List[Symbol],
                           supertype: ObjectType,
                           interfaceTypes: List[InterfaceType],
                           interfaceMethods: List[List[Symbol]],
                           methods: List[Symbol],
                           annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validateComponents(module: Module): List[CompileException] = {
    validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      supertype.validate(module, getLocation) ++
      interfaceTypes.flatMap(_.validate(module, getLocation)) ++
      interfaceMethods.flatMap(validateComponentsOfClass[Function](module, _)) ++
      validateComponentsOfClass[Function](module, methods)
  }

  override def validate(module: Module): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }
}
