package com.jayconrod.tungsten

final case class Symbol(val name: List[String],
                        val id: Int,
                        val location: Location)
{
  if (name.isEmpty || !name.forall(!_.isEmpty) || id < 0)
    throw new IllegalArgumentException

  def this(simpleName: String) = this(List(simpleName), 0, Nowhere)
  def this(simpleName: String, id: Int, loc: Location) = this(List(simpleName), id, loc)

  override def equals(that: Any) = {
    that match {
      case Symbol(n, i, l) if name == n && id == i && location == l => true
      case _ => false
    }
  }

  override def hashCode = {
    val parts = List[Any](name, id, location)
    parts.foldLeft(0)(Utilities.hash)
  }
}
