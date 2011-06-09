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

import Lexer.Token

trait EscapeTranslation {
  def translateEscapes(chars: String): String = {
    val buffer = new StringBuffer
    var index = 0
    while (index < chars.length) {
      val next = chars(index)
      index += 1
      if (next != '\\')
        buffer.append(next)
      else {
        def hex(ch: Char): Int = {
          if ('0' <= ch && ch <= '9')
            ch - '0'
          else if ('A' <= ch && ch <= 'F')
            ch - 'A' + 10
          else
            throw new RuntimeException("must be hex character")
        }
        val unescapedValue = hex(chars(index)) << 4 | hex(chars(index + 1))
        index += 2
        val unescaped = unescapedValue.toChar
        buffer.append(unescaped)
      }
    }
    buffer.toString
  }
}

final case class ReservedToken(chars: String)
  extends Token

final case class StringToken(chars: String)
  extends Token
  with EscapeTranslation
{
  if (chars.head != '"' || chars.last != '"')
    throw new IllegalArgumentException

  def value: String = translateEscapes(chars.substring(1, chars.length - 1))
}

final case class IntToken(chars: String) 
  extends Token
{
  def value: Long = chars.toLong
}

final case class IntTypeToken(chars: String)
  extends Token
{
  def width: Int = chars.substring(1).toInt
}

final case class FloatToken(chars: String)
  extends Token
{
  def value: Double = chars.toDouble
}

final case class SymbolToken(chars: String)
  extends Token
  with EscapeTranslation
{
  if (chars.size < 2 || (chars.head != '%' && chars.head != '@') ||
      (chars(1) == '%' && (chars.size < 3 || chars(chars.size - 1) != '%')))
    throw new IllegalArgumentException

  def isGlobal: Boolean = chars.head == '@'

  def value: String = {
    if (chars(1) == '"')
      chars(0) + translateEscapes(chars.substring(2, chars.size - 1))
    else
      chars(0) + chars.substring(1)
  }
}

final case class LabelToken(chars: String)
  extends Token
{
  def value: String = '%' + chars
}
