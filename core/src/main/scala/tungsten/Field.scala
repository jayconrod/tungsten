package tungsten

final case class Field(override name: Symbol, 
                       ty: Type, 
                       override annotations: List[AnnotationValue] = Nil,
                       override location: Location = Nowhere)
  extends Definition(name, annotations, location)
  with TypedDefinition
{
  def ty(module: Module) = ty

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module)
  }
}

                  
