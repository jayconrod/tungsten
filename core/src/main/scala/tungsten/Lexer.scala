package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharArrayReader
import scala.util.matching.Regex
import Utilities._

object Lexer extends Lexical with RegexParsers {
  override type Elem = Char

  val reservedOperators = Set[String]()
  val reservedWords = Set[String]()

  override def whitespaceChar: Parser[Elem] = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  def comment: Parser[Any] = elem(';') ~ rep(chrExcept('\n') ~ elem('\n'))

  def whitespace: Parser[Any] = rep(whitespaceChar | comment)

  override def errorToken(message: String): Token = {
    ErrorTok(message)
  }

  def integer: Parser[Long] = {
    opt('-') ~ rep1(digit) ^^ {
      case sign ~ digits => {
        val intStr = sign.map(_.toString).getOrElse("") + digits.mkString
        intStr.toLong
      }
    }
  }

  def token: Parser[Token] = {
    (integer ^^ { v => IntTok(v) })
  }

  def test(input: String) = {
    val reader = new CharArrayReader(input.toArray)
    phrase(token)(reader).get
  }      
}