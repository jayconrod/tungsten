package tungsten

import Utilities._

final class Symbol(val name: Iterable[String], val id: Int)
{
  if (name.isEmpty || !name.forall(!_.isEmpty) || id < 0)
    throw new IllegalArgumentException

  def this(simpleName: String) = this(List(simpleName), 0)
  def this(simpleName: String, id: Int) = this(List(simpleName), id)
  def this(name: Iterable[String]) = this(name, 0)

  def + (right: String) = new Symbol(name ++ List(right), id)
  def + (right: Symbol) = new Symbol(name ++ right.name, right.id)

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

object Symbol {
  def apply(simpleName: String) = new Symbol(simpleName)
  def apply(simpleName: String, id: Int) = new Symbol(simpleName, id)
  def apply(name: Iterable[String]) = new Symbol(name)
  def apply(name: Iterable[String], id: Int) = new Symbol(name, id)

  def unapply(that: Any): Option[(List[String], Int)] = {
    if (!that.isInstanceOf[Symbol])
      None
    else {
      val sym = that.asInstanceOf[Symbol]
      Some(sym.name.toList, sym.id)
    }
  }      

  implicit def fromString(string: String) = {
    import scala.util.parsing.input.CharArrayReader
    val reader = new CharArrayReader(string.toCharArray)
    val result = AstLexer.phrase(AstLexer.symbol)(reader)
    result match {
      case AstLexer.Success(sym, _) => sym
      case _ => throw new IllegalArgumentException
    }
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
