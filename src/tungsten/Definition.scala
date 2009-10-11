package tungsten

abstract class Definition(val name: Symbol, location: Location = Nowhere) 
  extends TungstenObject(location)
{
  def toString: String
}

trait TypedDefinition extends Definition {
  def ty(module: Module): Type
}
