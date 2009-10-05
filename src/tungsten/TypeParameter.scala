package tungsten

final case class TypeParameter(override name: Symbol, 
                               upperBound: Option[Type] = None,
                               lowerBound: Option[Type] = None,
                               override location: Location = Nowhere) 
  extends Definition(name, location)
{
  override def toString = {
    val upperBoundStr = upperBound match {
      case Some(u) => " <: " + u
      case None    => ""
    }
    val lowerBoundStr = lowerBound match {
      case Some(l) => " >: " + l
      case None    => ""
    }
    name.toString + upperBoundStr + lowerBoundStr
  }
}
