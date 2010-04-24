package tungsten

final case class Annotation(override name: Symbol,
                            fields: List[Symbol],
                            override annotations: List[AnnotationValue],
                            override location: Location = Nowhere)
  extends Definition(name, annotations, location)
{
  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Field](module, fields)
  }
}

final case class AnnotationValue(name: Symbol, fields: List[Value])
