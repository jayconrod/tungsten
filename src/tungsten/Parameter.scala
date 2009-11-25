package tungsten

final case class Parameter(override name: Symbol, 
                           ty: Type, 
                           override location: Location = Nowhere)
  extends Definition(name, location) with TypedDefinition
{
  def ty(module: Module) = ty

  def validate(module: Module) = ty.validate(module)

  override def equals(that: Any) = {
    that match {
      case Parameter(n, t, _) if name == n && ty == t => true
      case _ => false
    }
  }

  override def hashCode = Utilities.hash("parameter", name, ty)

  override def toString = name.toString + ": " + ty
}
