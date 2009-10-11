package tungsten

final case class Parameter(override name: Symbol, 
                           ty: Type, 
                           override location: Location = Nowhere)
  extends Definition(name, location)
{
  def validate(module: Module) = ty.validate(module)

  override def toString = name.toString + ": " + ty
}
