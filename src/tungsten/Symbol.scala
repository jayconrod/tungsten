package tungsten

import Utilities._

final case class Symbol(val name: Iterable[String], val id: Int)
{
  if (name.isEmpty || !name.forall(!_.isEmpty) || id < 0)
    throw new IllegalArgumentException

  def this(simpleName: String) = this(List(simpleName), 0)
  def this(simpleName: String, id: Int) = this(List(simpleName), id)

  override def equals(that: Any) = {
    that match {
      case Symbol(n, i) if name == n && id == i => true
      case _ => false
    }
  }

  override def hashCode = {
    val parts = List[Any](name, id)
    parts.foldLeft(0)(hash _)
  }

  override def toString = {
    val concatenatedName = name.mkString(".")
    if (id == 0) concatenatedName else concatenatedName + "#" + id
  }
}

final class SymbolFactory {
  private var currentId = 0

  def complexSymbol(name: Iterable[String]) = {
    currentId += 1
    new Symbol(name, currentId)
  }

  def symbol(simpleName: String) = {
    complexSymbol(List(simpleName))
  }

  def nextId = {
    currentId += 1
    currentId
  }
}
