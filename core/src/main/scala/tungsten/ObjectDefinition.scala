/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

import Utilities._

trait ObjectDefinition 
  extends Definition
{
  def typeParameters: List[Symbol]

  final def getTypeParameters(module: Module): List[TypeParameter] = {
    module.getTypeParameters(typeParameters)
  }

  def getSuperType: Option[ObjectDefinitionType]
  def getSuperDefn(module: Module): Option[ObjectDefinition] = {
    getSuperType.map(_.getObjectDefinition(module))
  }
  def interfaceTypes: List[InterfaceType]
  def interfaceMethods: List[List[Symbol]]
  def interfaceDefns(module: Module): List[ObjectDefinition] = {
    interfaceTypes.map(_.getObjectDefinition(module))
  }
  def methods: List[Symbol]

  def selfType: ObjectDefinitionType

  def baseClass(module: Module): Class

  def inheritedTypes: List[ObjectDefinitionType] = {
    getSuperType match {
      case Some(superType) => superType :: interfaceTypes
      case None => interfaceTypes
    }
  }

  def inheritedDefns(module: Module): List[ObjectDefinition] = {
    getSuperDefn(module).toList ++ interfaceDefns(module)
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

  def substitutedInheritedTypes(typeArguments: List[Type]): List[ObjectDefinitionType] = {
    inheritedTypes.map(substituteInheritedType(_, typeArguments))
  }

  def getInheritedType(fromName: Symbol): ObjectDefinitionType = {
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

  def getIVTables(module: Module): Map[Symbol, Either[List[Symbol], Symbol]] = {
    getIVTables(methods, None, Map(), module)
  }

  def getIVTables(descendentMethods: List[Symbol],
                  descendentName: Option[Symbol],
                  ivtableMap: Map[Symbol, Either[List[Symbol], Symbol]],
                  module: Module): Map[Symbol, Either[List[Symbol], Symbol]] =
  {
    def makeInterfaceIVTable(ivtable: List[Symbol], 
                             interfaceMethods: List[Symbol]): List[Symbol] = 
    {
      interfaceMethods map { methodName => 
        val methodIndex = methods.indexOf(methodName)
        ivtable(methodIndex)
      }
    }

    if (ivtableMap.contains(name))
      ivtableMap
    else {
      assert(descendentMethods.size >= methods.size)
      val ivtableMethods = descendentMethods.take(methods.size)
      val ivtable = descendentName match {
        case Some(dn) => Right(dn)
        case None => Left(ivtableMethods)
      }
      val myIVTableMap = if (isInstanceOf[Interface])
        ivtableMap + (name -> ivtable)
      else
        ivtableMap
      val superIVTableMap = getSuperDefn(module) match {
        case Some(defn) 
          if !(isInstanceOf[Interface] && defn.isInstanceOf[Class]) => {
            defn.getIVTables(ivtableMethods, descendentName.orElse(Some(name)), 
                             myIVTableMap, module)
          }
        case _ => myIVTableMap
      }
      (superIVTableMap /: (0 until interfaceTypes.size)) { (ivtableMap, interfaceIndex) =>
        val methodNames = interfaceMethods(interfaceIndex)
        val interface = interfaceTypes(interfaceIndex).getObjectDefinition(module)
        val interfaceIVTable = makeInterfaceIVTable(ivtableMethods, methodNames)
        interface.getIVTables(interfaceIVTable, None, ivtableMap, module)
      }
    }
  }

  def validateInheritedMethods(parentType: ObjectDefinitionType,
                               methodNames: List[Symbol],
                               module: Module): List[CompileException] =
  {
    val methodDefns = module.getFunctions(methods)
    val parentDefn = module.getObjectDefinition(parentType.definitionName)
    val parentMethodDefns = module.getFunctions(parentDefn.methods)

    if (methodDefns.size < parentMethodDefns.size)
      List(MissingMethodException(name, getLocation))
    else {
      (methodDefns zip parentMethodDefns) flatMap { p =>
        val (method, parentMethod) = p
        validateOverride(method, parentMethod, module)
      }
    }
  }

  def validateOverride(method: Function,
                       overriddenMethod: Function,
                       module: Module): List[CompileException] =
  {
    /* Removes the "this" parameter, and all type parameters which correspond to type parameters
     * in the class. None is returned if the method doesn't have a "this" parameter, or if the
     * type parameters don't match the class it comes from.
     */
    def stripParameters(method: Function): Option[FunctionType] = {
      getThisParameterTypeForMethod(method, module) match {
        case Some(thisParamType) => {
          val returnType = method.returnType
          val methodClass = module.getObjectDefinition(thisParamType.definitionName)
          val methodTypeParameterNames = method.typeParameters.drop(methodClass.typeParameters.size)
          val parameterTypes = module.getParameters(method.parameters.tail).map(_.ty)
          Some(FunctionType(returnType, methodTypeParameterNames, parameterTypes))
        }
        case None => None
      }
    }

    if (overriddenMethod.isFinal)
      List(FinalMethodOverrideException(method.name, name, getLocation))
    else {
      (stripParameters(method), stripParameters(overriddenMethod)) match {
        case (Some(methodType), Some(overriddenType)) => {
          if (methodType.isSubtypeOf(overriddenType, module))
            Nil
          else
            List(MethodOverrideCompatibilityException(method.name, name, overriddenMethod.name, getLocation))
        }
        case _ => Nil // methods are not correctly formed; different errors will be reported
      }
    }
  }

  /** Performs validation for a single method. We are checking for the following:
   *  1) the method has a valid "this" parameter
   *    a) the method must have at least one argument
   *    b) the first argument must be a valid object type (type validation is performed elsewhere)
   *    c) if the object type corresponds to this class, each type argument must be a simple
   *       variable type, defined by a type parameter in the method. The type parameters must
   *       be at the beginning of the method type parameter list, and they must have the same
   *       bounds as the corresponding ones in the class.
   *    d) If the object type is not of this class, it must be for a superclass or interface.
   *  3) if the method's "this" type is of a different class, that class must define the method
   *     in the same position in the method list as this class. This is necessary to make the
   *     vtable work.
   */
  def validateMethod(method: Function, 
                     methodIndex: Int, 
                     module: Module): List[CompileException] = 
  {
    def validateTypeParametersMatch = {
      if (methodHasMatchingTypeParameters(method, module))
        Nil
      else
        List(MethodTypeParameterMismatchException(method.name, name, method.getLocation))
    }

    def validateThisParameter = {
      if (isThisParameterValid(method, module))
        Nil
      else
        List(MethodSelfTypeException(method.name, name, method.getLocation))
    }

    def validateMethodInheritance = {
      val thisParamTy = getThisParameterTypeForMethod(method, module).get
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

    stage(validateTypeParametersMatch,
          validateThisParameter,
          validateMethodInheritance)
  }    

  def getThisParameterTypeForMethod(method: Function, module: Module): Option[ObjectDefinitionType] = {
    method.parameters.headOption.map(module.getParameter _) collect {
      case Parameter(_, ty: ObjectDefinitionType, _) => ty
    }
  }

  def isMethodDefinedInThisClass(method: Function, module: Module): Boolean = {
    getThisParameterTypeForMethod(method, module) match {
      case Some(thisParameterType) if thisParameterType.definitionName == name => true
      case _ => false
    }
  }

  def isThisParameterValid(method: Function, module: Module): Boolean = {
    getThisParameterTypeForMethod(method, module) match {
      case None => false
      case Some(thisParameterType) => {
        if (thisParameterType.definitionName != name) {
          // Since this method comes from a different class, it will get checked there. We
          // just need to make sure this is actually a superclass.
          isDescendedFrom(thisParameterType.definitionName, module)
        } else {
          // We only check the "this" parameter type if the method is from this class.
          isThisParameterValidForThisClass(method, module)
        }
      }
    }
  }

  def isThisParameterValidForThisClass(method: Function, module: Module): Boolean = {
    getThisParameterTypeForMethod(method, module) match {
      case Some(thisParameterType) if thisParameterType.definitionName == name => {
        val thisParameterType = getThisParameterTypeForMethod(method, module).get
        val classTypeParameters = module.getTypeParameters(typeParameters)
        val methodTypeParameters = module.getTypeParameters(method.typeParameters)
        val thisTypeArguments = thisParameterType.typeArguments
        if (thisTypeArguments.size != classTypeParameters.size ||
            thisTypeArguments.size > methodTypeParameters.size)
          false
        else {
          (thisTypeArguments zip methodTypeParameters) forall { p =>
            val (thisTypeArgument, methodTypeParameter) = p
            thisTypeArgument match {
              case VariableType(typeParameterName) if typeParameterName == methodTypeParameter.name =>
                true
              case _ => false
            }
          }
        }
      }
      case _ => false
    }
  }

  def methodHasMatchingTypeParameters(method: Function, module: Module): Boolean = {
    val methodTypeParameters = module.getTypeParameters(method.typeParameters)
    val classTypeParameters = module.getTypeParameters(typeParameters)
    if (methodTypeParameters.size < classTypeParameters.size)
      false
    else {
      (methodTypeParameters zip classTypeParameters) forall { p =>
        val (methodTypeParameter, classTypeParameter) = p
        methodTypeParameter.boundsMatch(classTypeParameter)
      }
    }
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

    validateIndividualMethods   ++ 
      validateInterfaceMethods  ++
      validateParentInheritance ++
      validateInterfaces
  }

  def validateParentNotFinal(module: Module): List[CompileException] = {
    getSuperType match {
      case Some(ClassType(superclassName, _)) => {
        val superclass = module.getClass(superclassName)
        if (superclass.isFinal)
          List(FinalClassInheritanceException(name, superclassName, getLocation))
        else
          Nil
      }
      case _ => Nil
    }
  }
}

