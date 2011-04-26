package tungsten

final case class Interface(name: Symbol,
                           typeParameters: List[Symbol],
                           supertype: ObjectDefinitionType,
                           interfaceTypes: List[InterfaceType],
                           interfaceMethods: List[List[Symbol]],
                           methods: List[Symbol],
                           annotations: List[AnnotationValue] = Nil)
  extends Definition
  with ObjectDefinition
{
  override def isGlobal = true

  override def validateComponents(module: Module): List[CompileException] = {
    super.validateComponents(module) ++
      validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      interfaceMethods.flatMap(validateComponentsOfClass[Function](module, _)) ++
      validateComponentsOfClass[Function](module, methods)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    validateTypeAndValueScope(scope) ++
      validateComponentsScope(module, scope ++ typeParameters, typeParameters)
  }

  override def validate(module: Module): List[CompileException] = {
    validateMethods(module) ++
      validateParentNotFinal(module)
  }

  def getSuperType: Option[ObjectDefinitionType] = Some(supertype)

  def selfType: InterfaceType = {
    InterfaceType(name, typeParameters.map { t => VariableType(t) })
  }

  def getParentClass(module: Module): Class = {
    supertype.getObjectDefinition(module) match {
      case c: Class => c
      case i: Interface => i.getParentClass(module)
    }
  }
}
