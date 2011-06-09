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

package tungsten.llvm

import scala.util.matching.Regex

object Utilities {
  def hexDigit(value: Int): Char = {
    assert(0 <= value && value < 16)
    val hex = if (value < 10)
      '0' + value
    else
      'A' + value - 10
    hex.toChar
  }

  def escapeIdentifier(idStr: String): String = {
    val (prefix, id) = if (idStr.head == '%' || idStr.head == '@')
      (idStr.head, idStr.substring(1))
    else
      ("", idStr)
    identifierRegex.findFirstIn(id) match {
      case Some(m) if m == id => prefix + id
      case _ if id.forall(_.isDigit) => prefix + id
      case _ => prefix + escapeString(id)
    }
  }

  def escapeString(str: String): String = {
    val buffer = new StringBuffer
    buffer.append('"')
    for (ch <- str) {
      if (ch != '"' && ' ' <= ch && ch <= '~')
        buffer.append(ch)
      else if (ch >= 128)
        buffer.append('?')
      else {
        val ord = ch.toInt
        buffer.append('\\')
        buffer.append(hexDigit(ord >> 4 & 0xF))
        buffer.append(hexDigit(ord & 0xF))
      }
    }
    buffer.append('"')
    buffer.toString
  }

  lazy val identifierRegex = new Regex("[A-Za-z._$][A-Za-z0-9._$]*")
}
