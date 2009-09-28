package tungsten

final case class Global(override name: Symbol,
                        ty: Type,
                        value: Option[Value],
                        override location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = "global " + name + ": " + ty
}
