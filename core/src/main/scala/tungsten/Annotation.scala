package tungsten

final case class Annotation(name: Symbol,
                            parameters: List[Symbol],
                            annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Parameter](module, parameters)
  }
}

final case class AnnotationValue(name: Symbol, values: List[Value])
  extends Copying[AnnotationValue]
