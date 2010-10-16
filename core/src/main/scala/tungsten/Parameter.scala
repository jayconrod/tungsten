package tungsten

final case class Parameter(name: Symbol, 
                           ty: Type,
                           annotations: List[AnnotationValue] = Nil)
  extends Definition
