package tungsten

final case class Parameter(override name: Symbol, 
                           ty: Type,
                           override annotations: List[AnnotationValue] = Nil,
                           override location: Location = Nowhere)
  extends Definition(name, annotations, location)
{
  def ty(module: Module): Type = ty

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module)
  }

  override def equals(that: Any) = {
    that match {
      case Parameter(n, t, as, _) if name == n && ty == t && annotations == as => true
      case _ => false
    }
  }

  override def hashCode = Utilities.hash("parameter", name, ty)
}
