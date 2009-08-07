package com.jayconrod.tungsten

import Utilities._

final case class Symbol(val name: Iterable[String],
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
    parts.foldLeft(0)(hash _)
  }

  override def toString = {
    val concatenatedName = joinStrings(".", name)
    if (id == 0) concatenatedName else concatenatedName + "#" + id
  }
}

final class SymbolFactory {
  private var currentId = 0

  def complexSymbol(name: Iterable[String], location: Location = Nowhere) = {
    currentId += 1
    new Symbol(name, currentId, location)
  }

  def symbol(simpleName: String, location: Location = Nowhere) = {
    complexSymbol(List(simpleName), location)
  }

  def nextId = {
    currentId += 1
    currentId
  }
}
