package tungsten.llvm

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input._
import scala.util.matching.Regex
import Utilities._

object Lexer extends Lexical with RegexParsers {
  override type Elem = Char

  val reservedOperators = Set("=", ":", "{", "}", "(", ")", "[", "]", "*", ",")
  val reservedWords = Set("datalayout", "define", "nounwind", "target", "triple", "to",
                          "align", "label", "void",
                          "alloca", "bitcast", "br", "extractvalue", "insertvalue", "phi", 
                            "load", "ret", "store", "unreachable",
                          "zeroext", "signext", "inreg", "byval", "sret", "noalias", 
                            "nocapture", "nest",
                          "alwaysinline", "inlinehint", "optsize", "noreturn", "nounwind",
                            "readnone", "readonly", "ssp", "sspreq", "noredzone", 
                            "noimplicithint", "naked")

  override def whitespaceChar: Parser[Elem] = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  def comment: Parser[Any] = elem(';') ~ rep(chrExcept('\n')) ~ elem('\n')

  def whitespace: Parser[Any] = rep(whitespaceChar | comment)

  override def errorToken(message: String): Token = {
    ErrorToken(message)
  }

  def word: Parser[Token] = {
    def checkReserved(s: String): Parser[Token] = {
      if (reservedWords(s))
        success(ReservedToken(s))
      else
        elem(':') ^^^ LabelToken(s)
    }
    regex(new Regex("[A-Za-z._$][A-Za-z0-9._$]*")) >> checkReserved
  }

  def operator: Parser[Token] = {
    def parseOperator(r: String): Parser[ReservedToken] = {
      accept(r.toList) ^^ { s => ReservedToken(s.mkString) }
    }
    val reservedArray = new Array[String](reservedOperators.size)
    reservedOperators.copyToArray(reservedArray, 0)
    scala.util.Sorting.quickSort(reservedArray)
    val reservedParsers = reservedArray.toList.map(parseOperator _)
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

  def integer: Parser[IntToken] = {
    opt('-') ~ rep1(digit) ^^ { 
      case sign ~ digits => {
        val signStr = sign.map(_.toString).getOrElse("")
        IntToken(signStr + digits.mkString)
      }
    }
  }

  def intType: Parser[IntTypeToken] = {
    elem('i') ~ rep1(digit) ^^ { case i ~ n => IntTypeToken(i + n.mkString) }
  }

  def symbol: Parser[SymbolToken] = {
    def normalSymbol = identifier
    def quotedSymbol = elem('"') ~ rep1(chrExcept('"')) ~ elem('"') ^^ {
      case q1 ~ s ~ q2 => q1 + s.mkString + q2
    }
    def numericSymbol = rep1(digit) ^^ { _.mkString }
    (elem('%') | elem('@')) ~ (normalSymbol | quotedSymbol | numericSymbol) ^^ { 
      case prefix ~ sym => SymbolToken(prefix + sym) }
  }

  def label: Parser[LabelToken] = {
    (identifier <~ ':' ^^ { case s => LabelToken(s) }) |
    (string <~ ':' ^^ { case s => LabelToken(s.value) })
  }

  def identifier: Parser[String] = regex(identifierRegex)

  def token: Parser[Token] = {
    operator |
    word     |
    label    |
    string   |
    integer  |
    intType  |
    symbol
  }

  def test(input: String) = {
    val reader = new CharArrayReader(input.toArray)
    phrase(token)(reader) match {
      case Success(tok, _) => tok
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}
