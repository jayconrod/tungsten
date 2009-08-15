package tungsten

final class Global(name:      Symbol,
                   val ty:    Type,
                   val value: Option[Value],
                   location:  Location = Nowhere)
{
  override def toString = "global " + name + ": " + ty
}
