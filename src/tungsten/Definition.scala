package tungsten

abstract class Definition(val name: Symbol, location: Location = Nowhere) 
  extends TungstenObject(location)
{
  def toString: String
}
