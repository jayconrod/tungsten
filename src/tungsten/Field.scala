package tungsten

final case class Field(override name: Symbol, ty: Type, override location: Location = Nowhere)
  extends Definition(name, location)
  with TypedDefinition
{
  def ty(module: Module) = ty

  def validate(module: Module) = {
    ty.validate(module)
  }

  override def toString = {
    "field " + name + ": " + ty
  }
}

                  
