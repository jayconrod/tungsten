/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

import Utilities._

final class Symbol(val name: List[String], val id: Int)
  extends Ordered[Symbol]
{
  if (name.isEmpty || !name.forall(!_.isEmpty) || id < 0)
    throw new IllegalArgumentException

  def copy(name: List[String] = name, id: Int = id) = new Symbol(name, id)

  def + (right: String) = new Symbol(name ++ List(right), id)
  def + (right: Symbol) = new Symbol(name ++ right.name, right.id)
  def + (id: Int) = new Symbol(name, id)

  def simple = Symbol(name.last, id)

  def isSimple = name.size == 1

  def parent = {
    assert(name.size >= 2)
    Symbol(name.take(name.size - 1))
  }    

  def withoutPrefix(prefix: Symbol): Option[Symbol] = {
    def removePrefix(prefixName: List[String], name: List[String]): Option[List[String]] = {
      (prefixName, name) match {
        case (Nil, Nil) => None
        case (Nil, rest) => Some(rest)
        case (p :: pRest, n :: nRest) if p == n => removePrefix(pRest, nRest)
        case _ => None
      }
    }
    removePrefix(prefix.name, name).map { n => new Symbol(n, id) }
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

final class SymbolFactory(private var currentId: Int) {
  def this() = this(0)

  def apply(name: Symbol) = complexSymbol(name.name)
  def apply(name: List[String]) = complexSymbol(name)
  def apply(name: String) = symbol(name)

  def complexSymbol(name: List[String]) = {
    new Symbol(name, nextId)
  }

  def symbol(simpleName: String) = {
    complexSymbol(List(simpleName))
  }

  def nextId = {
    if (Integer.MAX_VALUE == currentId)
      throw new IllegalStateException("largest possible symbol ID already created!")
    currentId += 1
    currentId
  }
}
