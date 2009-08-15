package tungsten

final class Field(name: Symbol, val ty: Type, location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    "field " + name + ": " + ty
  }
}

                  
