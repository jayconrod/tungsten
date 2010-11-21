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
    val parentFieldTypes = superclass match {
      case Some(ClassType(superclassName, superclassTypeArgs)) => {
        val superclassDefn = module.getClass(superclassName)
        val superclassFieldTypes = module.getFields(superclassDefn.fields).map(_.ty)
        superclassFieldTypes.map(superclassDefn.substituteInheritedType(_, superclassTypeArgs))
      }
      case None => Nil
    }
    val fieldDefns = module.getFields(fields)

    def validateFields = {
      if (fieldDefns.size < parentFieldTypes.size)
        List(MissingFieldException(name, getLocation))
      else {
        (fieldDefns zip parentFieldTypes) collect { 
          case (fieldDefn, parentFieldType) if fieldDefn.ty != parentFieldType =>
            TypeMismatchException(parentFieldType, fieldDefn.ty, fieldDefn.getLocation)
        }
      }
    }

    validateFields ++ validateMethods(module)
  }

  def getSuperType: Option[ClassType] = superclass

  def selfType: ObjectType = {
    ClassType(name, typeParameters.map { t => VariableType(t) })
  }

  def isSubclassOf(clas: Class, module: Module): Boolean = {
    if (this == clas)
      true
    else {
      superclass match {
        case None => false
        case Some(ClassType(superName, _)) => {
          val supr = module.getClass(superName)
          supr.isSubclassOf(clas, module)
        }
      }
    }
  }
}
