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
      case Some(AnnotationValue(_, fields)) => fields match {
        case List(StringValue(filename),
                  IntValue(beginLine, 32),
                  IntValue(beginColumn, 32),
                  IntValue(endLine, 32),
                  IntValue(endColumn, 32)) => {
          Location(filename, beginLine.toInt, beginColumn.toInt, endLine.toInt, endColumn.toInt)
        }
        case _ => Nowhere
      }
      case _ => Nowhere
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
