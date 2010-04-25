package tungsten

import Utilities._

abstract class Definition(val name: Symbol, 
                          val annotations: List[AnnotationValue])
{
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

  def validateComponents(module: Module): List[CompileException] = {
    validateComponentsOfClass[Annotation](module, annotations.map(_.name)) ++
      annotations.flatMap(_.fields).flatMap(_.validateComponents(module, getLocation))
  }

  def validate(module: Module): List[CompileException] = {
    def validateFieldCount(av: AnnotationValue) = {
      val ann = module.getAnnotation(av.name)
      val given = av.fields.size
      val required = ann.fields.size
      if (given != required)
        List(AnnotationFieldCountException(ann.name, given, required, getLocation))
      else
        Nil
    }

    def validateFieldTypes(av: AnnotationValue) = {
      val ann = module.getAnnotation(av.name)
      val fieldTypes = module.getFields(ann.fields).map(_.ty)
      av.fields.zip(fieldTypes).flatMap { vt => vt._1.validateType(vt._2, module, getLocation) }
    }

    stage(annotations.flatMap(validateFieldCount _),
          annotations.flatMap(validateFieldTypes _))
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
