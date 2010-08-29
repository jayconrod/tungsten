package tungsten

final case class Interface(name: Symbol,
                           typeParameters: List[Symbol],
                           superclass: ClassType,
                           interfaces: List[InterfaceType],
                           methods: List[Symbol],
                           annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validateComponents(module: Module): List[CompileException] = {
    validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      superclass.validate(module, getLocation) ++
      interfaces.flatMap(_.validate(module, getLocation)) ++
      validateComponentsOfClass[Function](module, methods)
  }

  override def validate(module: Module): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }
}
