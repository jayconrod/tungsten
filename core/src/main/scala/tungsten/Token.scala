package tungsten

import Utilities._

sealed abstract class Token

final case class ErrorToken(msg: String) extends Token {
  override def equals(that: Any) = that.isInstanceOf[ErrorToken]
  override val hashCode = "ErrorToken".hashCode
}
final case class ReservedToken(text: String) extends Token
final case class SymbolToken(symbol: Symbol) extends Token
final case class VersionToken(version: Version) extends Token
final case class ModuleDependencyToken(dependency: ModuleDependency) extends Token

sealed abstract class IntegerToken extends Token
final case class ByteToken(value: Byte) extends IntegerToken
final case class ShortToken(value: Short) extends IntegerToken
final case class IntToken(value: Int) extends IntegerToken
final case class LongToken(value: Long) extends IntegerToken

sealed abstract class FloatToken extends Token
final case class Float32Token(value: Float) extends FloatToken
final case class Float64Token(value: Double) extends FloatToken

final case class StringToken(value: String) extends Token

import Lexer.{Token => Tok}

final case class ErrorTok(msg: String) extends Tok {
  def chars = throw new UnsupportedOperationException
  override def equals(that: Any) = that.isInstanceOf[ErrorTok]
  override def hashCode = "ErrorToken".hashCode
}

final case class ReservedTok(text: String) extends Tok {
  def chars = text
}

final case class SymbolTok(symbol: Symbol) extends Tok {
  def chars = symbol.toString
}

final case class IntTok(value: Long) extends Tok {
  def chars = value.toString
}

final case class FloatTok(value: Double) extends Tok {
  def chars = value.toString
}

final case class CharTok(value: Char) extends Tok {
  def chars = {
    val charStr = if (charIsPrintable(value)) value.toString else "%04x".format(value.toInt)
    "'" + charStr + "'"
  }
}

final case class StringTok(value: String) extends Tok {
  def chars = {
    val buffer = new StringBuffer
    buffer.append("\"")
    for (ch <- value) {
      if (charIsPrintable(ch))
        buffer.append(ch)
      else
        buffer.append("%04x".format(ch.toInt))
    }
    buffer.append("\"")
    buffer.toString
  }
}

final case class VersionTok(value: Version) extends Tok {
  def chars = value.toString
}

final case class ModuleDependencyTok(value: ModuleDependency) extends Tok {
  def chars = value.toString
}
