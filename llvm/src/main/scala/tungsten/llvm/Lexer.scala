package tungsten.llvm

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input._
import scala.util.matching.Regex

object Lexer extends Lexical with RegexParsers {
  override type Elem = Char

  val reservedStrings = Set("=", ":", "{", "}", "(", ")", "*", ",",
                            "datalayout", "define", "nounwind", "target", "triple", "to",
                              "align", "label",
                            "alloca", "bitcast", "br", "load", "ret", "store")

  override def whitespaceChar: Parser[Elem] = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  def comment: Parser[Any] = elem(';') ~ rep(chrExcept('\n')) ~ elem('\n')

  def whitespace: Parser[Any] = rep(whitespaceChar | comment)

  override def errorToken(message: String): Token = {
    ErrorToken(message)
  }

  def reserved: Parser[Token] = {
    def parseReserved(r: String): Parser[ReservedToken] = {
      accept(r.toList) ^^ { s => ReservedToken(s.mkString) }
    }
    val reservedArray = new Array[String](reservedStrings.size)
    reservedStrings.copyToArray(reservedArray, 0)
    scala.util.Sorting.quickSort(reservedArray)
    val reservedParsers = reservedArray.toList.map(parseReserved)
    val fail: Parser[ReservedToken] = failure("no matching reserved string")
    (fail /: reservedParsers) {(x, y) => y | x}
  }

  def hexChar: Parser[Char] = {
    elem("hex character", { c => ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') })
  }  

  def stringChar: Parser[List[Char]] = {
    (elem('\\') ~ repN(2, hexChar) ^^ { case bs ~ hex => bs :: hex }) |
    (chrExcept('"', '\n') ^^ { (c: Char) => List(c) })
  }

  def string: Parser[StringToken] = {
    elem('"') ~ rep(stringChar) ~ elem('"') ^^ { 
      case q1 ~ s ~ q2 => StringToken(q1 + s.flatten.mkString + q2)
    }
  }

  def intType: Parser[IntTypeToken] = {
    elem('i') ~ rep1(digit) ^^ { case i ~ n => IntTypeToken(i + n.mkString) }
  }

  def symbol: Parser[SymbolToken] = {
    def normalSymbol = regex(new Regex("[a-zA-Z$._][a-zA-Z$._0-9]*"))
    def quotedSymbol = elem('"') ~ rep1(chrExcept('"')) ~ elem('"') ^^ {
      case q1 ~ s ~ q2 => q1 + s.mkString + q2
    }
    (elem('%') | elem('@')) ~ (normalSymbol | quotedSymbol) ^^ { 
      case prefix ~ sym => SymbolToken(prefix + sym) }
  }

  def token: Parser[Token] = {
    reserved |
    string |
    intType |
    symbol
  }
}
