package tungsten

final case class Annotation(name: Symbol,
                            fields: List[Symbol],
                            annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Field](module, fields)
  }
}

final case class AnnotationValue(name: Symbol, fields: List[Value])
  extends Copying[AnnotationValue]
