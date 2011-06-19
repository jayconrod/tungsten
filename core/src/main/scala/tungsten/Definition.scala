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

abstract class Definition
  extends Copying[Definition]
{
  def name: Symbol

  def isGlobal: Boolean = false

  def annotations: List[AnnotationValue]

  def hasAnnotation(annotationName: Symbol): Boolean = {
    annotations.exists(_.name == annotationName)
  }

  def getLocation: Location = {
    val locationAnnotation = annotations.find(_.name == symbolFromString("tungsten.Location"))
    locationAnnotation match {
      case Some(AnnotationValue(_, List(StringValue(filename),
                                        IntValue(beginLine, _),
                                        IntValue(beginColumn, _),
                                        IntValue(endLine, _),
                                        IntValue(endColumn, _)))) => {
        FileLocation(filename, beginLine.toInt, beginColumn.toInt, endLine.toInt, endColumn.toInt)
      }
      case _ => SymbolLocation(name)
    }
  }

  def isAbstract = hasAnnotation("tungsten.Abstract")

  def isFinal = hasAnnotation("tungsten.Final")

  def validateComponents(module: Module): List[CompileException] = {
    validateComponentsOfClass[Annotation](module, annotations.map(_.name))
  }

  def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    validateTypeAndValueScope(scope)
  }

  def validate(module: Module): List[CompileException] = {
    def validateAnnotationArgumentCount(av: AnnotationValue) = {
      val ann = module.getAnnotation(av.name)
      val given = av.values.size
      val required = ann.parameters.size
      if (given != required)
        List(AnnotationArgumentCountException(ann.name, given, required, getLocation))
      else
        Nil
    }

    def validateAnnotationArgumentTypes(av: AnnotationValue) = {
      val ann = module.getAnnotation(av.name)
      val fieldTypes = module.getParameters(ann.parameters).map(_.ty)
      av.values.zip(fieldTypes).flatMap { vt => 
        val (v, t) = vt
        v.validate(module, getLocation) ++ checkType(v.ty, t, getLocation)
      }
    }

    stage(annotations.flatMap(validateAnnotationArgumentCount _),
          annotations.flatMap(validateAnnotationArgumentTypes _))
  }

  protected def validateComponentsOfClass[T <: Definition](module: Module,
                                                           componentNames: List[Symbol])
                                                          (implicit m: Manifest[T]) =
  {
    val className = humanReadableClassName[T]

    def check(componentNames: List[Symbol], 
              seen: Set[Symbol],
              errors: List[CompileException]): List[CompileException] =
    {
      componentNames match {
        case Nil => errors
        case n :: ns => {
          val newErrors = if (seen.contains(n))
            DuplicateComponentException(name, n, className, getLocation) :: errors
          else
            module.validateName[T](n, getLocation) ++ errors
          check(ns, seen + n, newErrors)
        }
      }
    }

    check(componentNames, Set(), Nil)
  }

  protected def validateComponentOfClass[T <: Definition](module: Module,
                                                          componentName: Symbol)
                                                         (implicit m: Manifest[T]) =
  {
    validateComponentsOfClass[T](module, List(componentName))
  }

  protected def validateNonEmptyComponentsOfClass[T <: Definition](module: Module,
                                                                   componentNames: List[Symbol])
                                                                   (implicit m: Manifest[T]) =
  {
    val className = humanReadableClassName[T]
    if (componentNames.isEmpty)
      List(EmptyComponentsException(name, className, getLocation))
    else
      validateComponentsOfClass[T](module, componentNames)
  }

  protected def validateComponentScope(module: Module, 
                                       scope: Set[Symbol], 
                                       symbol: Symbol): List[CompileException] =
  {
    module.definitions(symbol).validateScope(module, scope)
  }

  protected def validateComponentsScope(module: Module,
                                        scope: Set[Symbol],
                                        symbols: Traversable[Symbol]): List[CompileException] =
  {
    symbols.flatMap { symbol => validateComponentScope(module, scope, symbol) }.toList
  }

  protected def validateSymbolScope(errors: List[CompileException], 
                                    symbol: Symbol, 
                                    scope: Set[Symbol]): List[CompileException] = 
  {
    if (!scope(symbol)) {
      ScopeException(symbol, name, getLocation) :: errors
    } else
      errors
  }

  protected def validateTypeAndValueScope(scope: Set[Symbol]) = {
    def checkSymbol(errors: List[CompileException], symbol: Symbol) = 
      validateSymbolScope(errors, symbol, scope)

    def checkType(errors: List[CompileException], ty: Type) =
      ty.foldSymbols(errors, checkSymbol)

    def checkValue(errors: List[CompileException], value: Value) =
      value.foldSymbols(errors, checkSymbol)

    foldTypes(Nil, checkType) ++ foldValues(Nil, checkValue)
  }
}
