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
  def substituteInheritedType[T <: Type](ty: T, typeArguments: List[Type]): T = {
    val substitutions = typeParameters zip typeArguments
    (ty /: substitutions) { (ty, sub) =>
      val (tyParamName, argument) = sub
      ty.substitute(tyParamName, argument).asInstanceOf[T]
    }
  }

  def substitutedInheritedTypes(typeArguments: List[Type]): List[ObjectType] = {
    inheritedTypes.map(substituteInheritedType(_, typeArguments))
  }

  def getInheritedType(fromName: Symbol): ObjectType = {
    getSuperType match {
      case Some(t) if t.definitionName == fromName => t
      case _ => inheritedTypes.find(_.definitionName == fromName) match {
        case Some(t) => t
        case None => throw new IllegalArgumentException
      }
    }
  }

  def validateInheritedMethods(parentType: ObjectType,
                               methodNames: List[Symbol],
                               module: Module): List[CompileException] =
  {
    val methodDefns = module.getFunctions(methods)
    val parentDefn = module.getObjectDefinition(parentType.definitionName)
    val parentMethods = module.getFunctions(parentDefn.methods)
    val parentMethodTypes = parentMethods.map { method =>
      val ty = method.ty(module)
      parentDefn.substituteInheritedType(ty, parentType.typeArguments)
    }

    def isCompatibleWithParent(methodType: FunctionType, 
                               parentMethodType: FunctionType): Boolean =
    {
      val parameterCountEqual = methodType.parameterTypes.size == parentMethodType.parameterTypes.size
      val returnTypeIsCompatible = methodType.returnType.isSubtypeOf(parentMethodType.returnType, module)
      val parameterTypesAreCompatible = (methodType.parameterTypes.tail zip parentMethodType.parameterTypes.tail).
        forall { p => 
          val (paramTy, parentTy) = p
          parentTy.isSubtypeOf(paramTy, module) 
        }
      parameterCountEqual && returnTypeIsCompatible && parameterTypesAreCompatible
    }

    if (methodDefns.size < parentMethodTypes.size)
      List(MissingMethodException(name, getLocation))
    else {
      (methodDefns zip parentMethodTypes) collect {
        case (method, parentMethodType) if !isCompatibleWithParent(method.ty(module), parentMethodType) =>
          TypeMismatchException(method.ty(module), parentMethodType, getLocation)
      }
    }
  }

  def validateMethods(module: Module): List[CompileException] = {
    val methodDefns = module.getFunctions(methods)

    def hasThisArgument(method: Function): Boolean = {
      method.parameters.headOption match {
        case Some(thisParamName) => {
          val thisParam = module.getParameter(thisParamName)
          selfType.isSubtypeOf(thisParam.ty, module)
        }
        case None => false
      }
    }

    def validateThisParameters = {
      methodDefns collect { 
        case method if !hasThisArgument(method) =>
          MethodSelfTypeException(method.name, name, method.getLocation)
      }
    }

    def validateInterfaceMethods = {
      if (interfaceTypes.size != interfaceMethods.size)
        List(InterfaceTypeMethodMismatchException(name, getLocation))
      else {
        val methodNames = methods.toSet
        val interfaceNames = interfaceTypes.map(_.definitionName)
        (interfaceNames zip interfaceMethods) flatMap { p =>
          val (interfaceName, interfaceMethodList) = p
          interfaceMethodList collect { 
            case interfaceMethodName if !methodNames.contains(interfaceMethodName) => {
              ForeignInterfaceMethodException(interfaceMethodName,
                                              interfaceName,
                                              name,
                                              getLocation)
            }
          }
        }
      }
    }

    def validateParentInheritance = {
      getSuperType match {
        case Some(parentType) => validateInheritedMethods(parentType, methods, module)
        case None => Nil
      }
    }

    def validateInterfaces = {
      (interfaceTypes zip interfaceMethods) flatMap { p =>
        val (interfaceType, methodNames) = p
        validateInheritedMethods(interfaceType, methodNames, module)
      }
    }          

    validateThisParameters ++ 
      validateInterfaceMethods ++
      validateParentInheritance ++
      validateInterfaces
  }
}
