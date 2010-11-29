package tungsten

import Utilities._

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

  def isDescendedFrom(defnName: Symbol, module: Module): Boolean = {
    if (name == defnName)
      true
    else {
      inheritedTypes exists { ty =>
        val inheritedName = ty.definitionName
        val inheritedDefn = module.getObjectDefinition(inheritedName)
        inheritedDefn.isDescendedFrom(defnName, module)
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

  /** Performs validation for a single method. We are checking for the following:
   *  1) the method has a valid "this" parameter
   *    a) the method must have at least one argument
   *    b) the first argument must be a valid object type (type validation is performed elsewhere)
   *    c) if the object type corresponds to this class, each type argument must be a simple
   *       variable type, defined by a type parameter in the method. The bounds for this type
   *       parameter must be the same as the corresponding type parameter in this class. If the
   *       object type is not of this class, it must be for a superclass or interface. 
   *  2) if the method's "this" type is of a different class, that class must define the method
   *     in the same position in the method list as this class. This is necessary to make the
   *     vtable work.
   */
  def validateMethod(method: Function, 
                     methodIndex: Int, 
                     module: Module): List[CompileException] = 
  {
    def validateThisParameter = {
      val thisParameterIsValid = method.parameters.headOption match {
        case None => false  // no this parameter at all
        case Some(thisParamName) => {
          module.getParameter(thisParamName).ty match {
            case thisParamTy: ObjectType => {
              if (thisParamTy.definitionName != name) {
                // Since this method comes from a different class, it will get checked there. We
                // just need to make sure this is actually a superclass.
                isDescendedFrom(thisParamTy.definitionName, module)
              } else {
                // We only check the "this" parameter type if the method is from this class.
                val classTypeParameters = module.getTypeParameters(typeParameters)
                val thisTypeArguments = thisParamTy.typeArguments
                def typeArgumentIsValid(thisTypeArgument: Type, 
                                        classTypeParameter: TypeParameter): Boolean =
                {
                  thisTypeArgument match {
                    case VariableType(methodTypeParameterName) => {
                      val methodTypeParameter = module.getTypeParameter(methodTypeParameterName)
                      classTypeParameter.upperBound == methodTypeParameter.upperBound &&
                        classTypeParameter.lowerBound == methodTypeParameter.lowerBound
                    }
                    case _ => false
                  }
                }
                (thisTypeArguments zip classTypeParameters) forall { p =>
                  val (thisTypeArgument, classTypeParameter) = p
                  typeArgumentIsValid(thisTypeArgument, classTypeParameter)
                }
              }
            }
            case _ => false // "this" parameter not an object type
          }
        }
      }
      if (thisParameterIsValid)
        Nil
      else
        List(MethodSelfTypeException(method.name, name, method.getLocation))
    }

    def validateMethodInheritance = {
      val thisParamTy = module.getParameter(method.parameters.head).ty.asInstanceOf[ObjectType]
      val methodClassName = thisParamTy.definitionName
      val methodClassDefn = module.getObjectDefinition(methodClassName)
      if (methodClassDefn eq this)
        Nil // not inherited
      else if (methodClassDefn.methods.isDefinedAt(methodIndex) &&
               methodClassDefn.methods(methodIndex) == method.name)
        Nil // correctly inherited
      else
        List(MethodNotInheritedException(method.name, name, methodClassName, getLocation))
    }   

    stage(validateThisParameter,
          validateMethodInheritance)
  }    

  def validateMethods(module: Module): List[CompileException] = {
    val methodDefns = module.getFunctions(methods)

    def validateIndividualMethods = {
      ((0 until methodDefns.size).toList zip methodDefns) flatMap { p =>
        val (methodIndex, method) = p
        validateMethod(method, methodIndex, module)
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

    // def getMethodClass(method: Function): ObjectDefinition = {
    //   val thisParamTy = module.getParameter(method.parameters.head).ty.asInstanceOf[ObjectType]
    //   val methodClassName = thisParamTy.definitionName
    //   module.getObjectDefinition(methodClassName)
    // }

    // def isLocalOrInheritedFromParent(method: Function, methodIndex: Int): Boolean = {
    //   val methodClassDefn = getMethodClass(method)
    //   if (methodClassDefn == this)  // local case
    //     true
    //   else {    // inherited case
    //     methodClassDefn.methods.isDefinedAt(methodIndex) && 
    //       methodClassDefn.methods(methodIndex) == method.name
    //   }
    // }

    // /* Verifies that each method with a "this" parameter type that does not correspond to a 
    //  * different class actually comes from that class. It must be in the same position in the
    //  * method list as in this class.
    //  */
    // def validateInheritance = {
    //   ((0 until methodDefns.size).toList zip methodDefns) collect {
    //     case (methodIndex, method) if !isLocalOrInheritedFromParent(method, methodIndex) =>
    //       MethodNotInheritedException(method.name, name, getMethodClass(method).name, getLocation)
    //   }
    // }

    validateIndividualMethods ++ 
      validateInterfaceMethods ++
      validateParentInheritance ++
      validateInterfaces
  }
}

