package tungsten

final case class Field(override name: Symbol, 
                       ty: Type, 
                       override annotations: List[AnnotationValue] = Nil)
  extends Definition(name, annotations)
{
  def ty(module: Module): Type = ty

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation)
  }
}

                  
