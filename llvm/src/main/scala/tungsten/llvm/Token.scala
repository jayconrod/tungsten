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
