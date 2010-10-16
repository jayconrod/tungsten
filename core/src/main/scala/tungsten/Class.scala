package tungsten

final case class Class(name: Symbol,
                       typeParameters: List[Symbol],
                       superclass: Option[ClassType],
                       interfaceTypes: List[InterfaceType],
                       interfaceMethods: List[List[Symbol]],
                       constructors: List[Symbol],
                       methods: List[Symbol],
                       fields: List[Symbol],
                       annotations: List[AnnotationValue] = Nil)
  extends Definition
  with ObjectDefinition
{
  override def validateComponents(module: Module): List[CompileException] = {
    super.validateComponents(module) ++
      validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      interfaceMethods.flatMap(validateComponentsOfClass[Function](module, _)) ++
      validateComponentsOfClass[Function](module, constructors) ++
      validateComponentsOfClass[Function](module, methods) ++
      validateComponentsOfClass[Field](module, fields)
  }

  override def validate(module: Module): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }

  def getSuperType: Option[ClassType] = superclass
}
