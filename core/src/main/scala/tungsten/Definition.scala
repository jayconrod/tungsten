package tungsten

import Utilities._

abstract class Definition
  extends Copying[Definition]
{
  def name: Symbol

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
}
