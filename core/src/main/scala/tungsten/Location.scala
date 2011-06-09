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

sealed case class Location(val filename: String,
                           val beginLine: Int,
                           val beginColumn: Int,
                           val endLine: Int,
                           val endColumn: Int)
{
  if (filename.isEmpty ||
      beginLine < 1 || beginColumn < 1 || endLine < 1 || endColumn < 1 ||
      (beginLine > endLine) || (beginLine == endLine && beginColumn > endColumn))
  {
    throw new IllegalArgumentException
  }

  def this(filename: String, line: Int, column: Int) = {
    this(filename, line, column, line, column)
  }

  def combine(loc: Location) = {
    if (filename != loc.filename)
      throw new IllegalArgumentException
    new Location(filename, beginLine, beginColumn, loc.endLine, loc.endColumn)
  }

  final override def equals(that: Any) = {
    that match {
      case Location(fn, bl, bc, el, ec) if filename == fn &&
                                           beginLine == bl &&
                                           beginColumn == bc &&
                                           endLine == el &&
                                           endColumn == ec => true
      case _ => false
    }
  }

  final override def hashCode: Int = {
    val parts = List[Any](filename, beginLine, beginColumn, endLine, endColumn)
    parts.foldLeft(0)(Utilities.hash _)
  }

  final override def toString = {
    "<" + filename + ":" + beginLine + "." + beginColumn + "-" + endLine + "." + endColumn + ">"
  }
}

object Nowhere extends Location("NOWHERE", 1, 1, 1, 1)

