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

  override lazy val whitespaceChar: Parser[Elem] = {
    elem(' ') | elem('\t') | elem('\n') | elem('\r')
  }

  lazy val comment: Parser[Any] = elem(';') ~ rep(chrExcept('\n') ~ elem('\n'))

  lazy val whitespace: Parser[Any] = rep(whitespaceChar | comment)

  override def errorToken(message: String): Token = {
    ErrorTok(message)
  }

  lazy val integer: Parser[Long] = {
    opt('-') ~ rep1(digit) ^^ {
      case sign ~ digits => {
        val intStr = sign.map(_.toString).getOrElse("") + digits.mkString
        intStr.toLong
      }
    }
  }

  def char(except: Set[Char] = Set()): Parser[Char] = {
    def isPrintable(c: Char): Boolean = charIsPrintable(c) && !except(c) && c != '\\'
    val printableChar =  elem("printable character", isPrintable _)
    val escapedChar = regex("\\\\[0-9A-Fa-f]{1,4}"r) ^^ { (s: String) =>
      val digits = s.substring(1)
      val code = (0 /: digits) { (code, digit) => 
        val digitValue = if ('0' <= digit && digit <= '9')
          digit - '0'
        else if ('A' <= digit && digit <= 'F')
          digit - 'A' + 10
        else
          digit - 'a' + 10
        code * 16 + digitValue
      }
      code.toChar
    }
    printableChar | escapedChar
  }

  lazy val quotedChar: Parser[Char] = {
    elem('\'') ~> char(Set('\'')) <~ elem('\'')
  }

  lazy val quotedString: Parser[String] = {
    elem('"') ~> rep(char(Set('"'))) <~ elem('"') ^^ { case s => s.mkString }
  }

  lazy val symbol: Parser[Symbol] = {
    val prefix = elem('@') | elem('%')
    val element = regex("[A-Za-z_$][A-Za-z0-9_$]*"r) | quotedString
    val id = opt(elem('#') ~> integer) ^^ { case i => i.map(_.toInt).getOrElse(0) }
    prefix ~ rep1sep(element, '.') ~ id ^^ { 
      case p ~ es ~ i => {
        val prefixedElements = (p + es.head) :: es.tail
        Symbol(prefixedElements, i)
      }
    }
  }

  def token: Parser[Token] = {
    (integer      ^^ { v => IntTok(v) })    |
    (quotedChar   ^^ { v => CharTok(v) })   |
    (quotedString ^^ { v => StringTok(v) }) |
    (symbol       ^^ { v => SymbolTok(v) })
  }

  def test[T](input: String, parser: Parser[T]): T = {
    val reader = new CharArrayReader(input.toArray)
    phrase(parser)(reader).get
  }

  def test(input: String): Token = test(input, token)
}