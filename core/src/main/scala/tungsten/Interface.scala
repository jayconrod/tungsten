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
    def validateNullable(ty: ObjectDefinitionType): List[CompileException] = {
      if (ty.isNullable)
        List(NullableInheritanceException(name, getLocation))
      else
        Nil
    }

    super.validateComponents(module) ++
      validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      interfaceMethods.flatMap(validateComponentsOfClass[Function](module, _)) ++
      validateComponentsOfClass[Function](module, methods) ++
      validateNullable(supertype) ++
      interfaceTypes.flatMap(validateNullable _)
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

  def baseClass(module: Module): Class = {
    val superDefn = supertype.getObjectDefinition(module)
    superDefn.baseClass(module)
  }

  def getParentClass(module: Module): Class = {
    supertype.getObjectDefinition(module) match {
      case c: Class => c
      case i: Interface => i.getParentClass(module)
    }
  }
}
