package tungsten

final class TypeParameter(name: Symbol, 
                          val upperBound: Option[Type] = None,
                          val lowerBound: Option[Type] = None,
                          location: Location = Nowhere) 
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
    name + upperBoundStr + lowerBoundStr
  }
}
