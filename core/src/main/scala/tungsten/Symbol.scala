package tungsten

import Utilities._

final class Symbol(val name: List[String], val id: Int)
  extends Ordered[Symbol]
{
  if (name.isEmpty || !name.forall(!_.isEmpty) || id < 0)
    throw new IllegalArgumentException

  def + (right: String) = new Symbol(name ++ List(right), id)
  def + (right: Symbol) = new Symbol(name ++ right.name, right.id)

  def simple = Symbol(name.last, id)

  def isSimple = name.size == 1

  def parent = {
    assert(name.size >= 2)
    Symbol(name.take(name.size - 1))
  }    

  def compare(that: Symbol): Int = {
    def compareName(lname: Seq[String], rname: Seq[String]): Int = {
      if (lname.isEmpty) {
        if (rname.isEmpty) 
          0 
        else
          -1
      } else {
        if (rname.isEmpty) 
          1 
        else {
          val cmp = lname.head.compareTo(rname.head)
          if (cmp != 0)
            cmp
          else
            compareName(lname.tail, rname.tail)
        }
      }
    }
    val cmp = compareName(name, that.name)
    if (cmp == 0)
      id.compare(that.id)
    else
      cmp
  }

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
    val idRegex = "[A-Za-z_$][A-Za-z0-9_$]*"r
    def quoteName(n: String): String = {
      idRegex.findFirstIn(n) match {
        case Some(m) if m == n => n
        case _ => {
          val buffer = new StringBuffer
          buffer.append('"')
          for (c <- n) {
            if (charIsPrintable(c) && c != '"' && c != '\\')
              buffer.append(c)
            else
              buffer.append("\\%04x".format(c.toInt))
          }
          buffer.append('"')
          buffer.toString
        }
      }
    }
    val concatenatedName = name.map(quoteName _).mkString(".")
    if (id == 0) concatenatedName else concatenatedName + "#" + id
  }
}

object Symbol {
  def apply(simpleName: String) = new Symbol(List(simpleName), 0)
  def apply(simpleName: String, id: Int) = new Symbol(List(simpleName), id)
  def apply(name: List[String]) = new Symbol(name, 0)
  def apply(name: List[String], id: Int) = new Symbol(name, id)

  def unapply(that: Any): Option[(List[String], Int)] = {
    if (!that.isInstanceOf[Symbol])
      None
    else {
      val sym = that.asInstanceOf[Symbol]
      Some(sym.name, sym.id)
    }
  }      
}

final class SymbolFactory {
  private var currentId = 0

  def complexSymbol(name: List[String]) = {
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
