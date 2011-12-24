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
  with AggregateDefinition
{
  override def isGlobal = true

  override def validateComponents(module: Module): List[CompileException] = {
    def validateNullable(ty: ObjectDefinitionType): List[CompileException] = {
      if (ty.isNullable)
        List(NullableInheritanceException(name, getLocation))
      else
        Nil
    }

    super.validateComponents(module) ++
      validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      interfaceMethods.flatMap(validateComponentsOfClass[Function](module, _)) ++
      validateComponentsOfClass[Function](module, constructors) ++
      validateComponentsOfClass[Function](module, methods) ++
      validateComponentsOfClass[Field](module, fields) ++
      superclass.toList.flatMap(validateNullable _) ++
      interfaceTypes.flatMap(validateNullable _)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    val tpScope = scope ++ typeParameters
    validateTypeAndValueScope(tpScope) ++
      validateComponentsScope(module, tpScope, typeParameters) ++
      validateComponentsScope(module, tpScope, fields)
  }

  override def validate(module: Module): List[CompileException] = {
    val parentFieldTypes = superclass match {
      case Some(ClassType(superclassName, superclassTypeArgs, _)) => {
        val superclassDefn = module.getClass(superclassName)
        val superclassFieldTypes = module.getFields(superclassDefn.fields).map(_.ty)
        superclassFieldTypes.map(superclassDefn.substituteInheritedType(_, superclassTypeArgs))
      }
      case None => Nil
    }
    val fieldDefns = module.getFields(fields)

    def validateConstructors = {
      val constructorDefns = module.getFunctions(constructors)
      constructorDefns flatMap { validateConstructor(_, module) }
    }

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

    def validateAbstractMethods = {
      if (isAbstract)
        Nil
      else {
        val methodDefns = module.getFunctions(methods)
        methodDefns collect {
          case method if method.isAbstract => 
            AbstractMethodException(name, method.name, getLocation)
        }
      }
    }

    def validateAbstractFinal = {
      if (isAbstract && isFinal)
        List(AbstractFinalClassException(name, getLocation))
      else
        Nil
    }

    validateConstructors ++
      validateFields ++ 
      validateMethods(module) ++ 
      validateAbstractMethods ++ 
      validateAbstractFinal ++
      validateParentNotFinal(module)
  }

  def validateConstructor(constructor: Function, module: Module): List[CompileException] = {
    def validateTypeParametersMatch = {
      if (methodHasMatchingTypeParameters(constructor, module))
        Nil
      else
        List(ConstructorTypeParameterMismatchException(constructor.name, name, constructor.getLocation))
    }

    def validateThisParameter = {
      if (isThisParameterValidForThisClass(constructor, module))
        Nil
      else
        List(ConstructorSelfTypeException(constructor.name, name, constructor.getLocation))
    }

    def validateReturnType = {
      if (constructor.returnType == UnitType)
        Nil
      else
        List(ConstructorReturnTypeException(constructor.name, name, constructor.getLocation))
    }

    stage(validateTypeParametersMatch,
          validateThisParameter) ++
      validateReturnType
  }

  def getSuperType: Option[ClassType] = superclass

  def selfType: ClassType = {
    ClassType(name, typeParameters.map { t => VariableType(t) })
  }

  def baseClass(module: Module): Class = this

  def isSubclassOf(clas: Class, module: Module): Boolean = {
    if (this == clas)
      true
    else {
      superclass match {
        case None => false
        case Some(ClassType(superName, _, _)) => {
          val supr = module.getClass(superName)
          supr.isSubclassOf(clas, module)
        }
      }
    }
  }
}
