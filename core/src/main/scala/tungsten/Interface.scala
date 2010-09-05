package tungsten

final case class Interface(name: Symbol,
                           typeParameters: List[Symbol],
                           superclass: ClassType,
                           interfaceTypes: List[InterfaceType],
                           interfaceMethods: List[List[Symbol]],
                           methods: List[Symbol],
                           annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validateComponents(module: Module): List[CompileException] = {
    validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      superclass.validate(module, getLocation) ++
      interfaceTypes.flatMap(_.validate(module, getLocation)) ++
      interfaceMethods.flatMap(validateComponentsOfClass[Function](module, _)) ++
      validateComponentsOfClass[Function](module, methods)
  }

  override def validate(module: Module): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }
}
