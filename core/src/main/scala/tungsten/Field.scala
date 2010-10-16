package tungsten

final case class Field(name: Symbol, 
                       ty: Type, 
                       annotations: List[AnnotationValue] = Nil)
  extends Definition

                  
