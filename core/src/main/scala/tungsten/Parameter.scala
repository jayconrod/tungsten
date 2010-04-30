package tungsten

final case class Parameter(name: Symbol, 
                           ty: Type,
                           annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  def ty(module: Module): Type = ty

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation)
  }
}
