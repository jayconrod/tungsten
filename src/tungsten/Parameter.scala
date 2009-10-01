package tungsten

final class Parameter(name: Symbol, val ty: Type, location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = name.toString + ": " + ty
}
