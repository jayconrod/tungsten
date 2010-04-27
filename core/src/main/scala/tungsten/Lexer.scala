package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharArrayReader
import scala.util.matching.Regex
import Utilities._

object Lexer extends Lexical with RegexParsers {
  override type Elem = Char

  val reservedStrings = {
    Set("{", "}", "(", ")", "[", "]",
        "*", "?", "x", ",", "=", ":", "to",
        "/", "%", "+", "-", "<<", ">>", ">>>", "&", "^", "|",
        "<", "<=", ">", ">=", "==", "!=",
        "exit",
        "address", "assign", "binop", "branch", "cond", "fextend", "ftoi", "ftruncate", 
          "heap", "heaparray", "isextend", "itof", "itruncate", "izextend",
          "intrinsic", "load", "loadelement", "relop", "return", "scall", "stack",
          "stackarray", "store", "storeelement", "upcast",
        "unit", "boolean", "char", "string", "int8", "int16", "int32", "int64", "float32",
          "float64", "nulltype", "struct",
        "true", "false", "null")
  }
                            

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

  lazy val float: Parser[Double] = {
    val optSign = opt(elem('-') | elem('+')) ^^ { c => c.map(_.toString).getOrElse("") }
    val num = rep1(digit) ^^ { _.mkString }
    val optNum = opt(num) ^^ { _.getOrElse("") }
    val exp = (elem('e') | elem('E')) ~ optSign ~ num ^^ {
      case e ~ s ~ n => e + s + n
    }
    val optExp = opt(exp) ^^ { _.getOrElse("") }

    val float1 = optSign ~ (num <~ '.') ~ optNum ~ optExp ^^ {
      case s ~ n ~ f ~ e => s + n + '.' + f + e
    }
    val float2 = (optSign <~ '.') ~ num ~ optExp ^^ {
      case s ~ f ~ e => s + '.' + f + e
    }
    val float3 = optSign ~ num ~ exp ^^ {
      case s ~ n ~ e => s + n + e
    }
    (float1 | float2 | float3) ^^ { _.toDouble }
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

  /** This is not directly used by the lexer. Instead, Utilities.symbolFromString uses it
   *  to create symbols concisely in the compiler source code (mainly in tests).
   */
  def generalSymbol(prefixOptional: Boolean): Parser[Symbol] = {
    val prefix: Parser[String] = {
      if (prefixOptional)
        opt(elem('@') | elem('%')) ^^ { _.map(_.toString).getOrElse("") }
      else
        (elem('@') | elem('%')) ^^ { _.toString }
    }
    val element = regex("[A-Za-z_$][A-Za-z0-9_$]*"r) | quotedString
    val id = opt(elem('#') ~> integer) ^^ { case i => i.map(_.toInt).getOrElse(0) }
    prefix ~ rep1sep(element, '.') ~ id ^^ { 
      case p ~ es ~ i => {
        val prefixedElements = (p + es.head) :: es.tail
        Symbol(prefixedElements, i)
      }
    }
  }

  lazy val symbol: Parser[Symbol] = generalSymbol(false)

  lazy val reserved: Parser[String] = {
    def parseReserved(r: String): Parser[String] = {
      accept(r.toList) ^^ { _.mkString }
    }
    val reservedArray = new Array[String](reservedStrings.size)
    reservedStrings.copyToArray(reservedArray, 0)
    scala.util.Sorting.quickSort(reservedArray)
    val reservedParsers = reservedArray.toList.map(parseReserved _)
    val fail: Parser[String] = failure("no matching reserved string")
    (fail /: reservedParsers) { (res, p) => p | res }
  } 

  def token: Parser[Token] = {
    (float        ^^ { v => FloatTok(v) })    |
    (integer      ^^ { v => IntTok(v) })      |
    (quotedChar   ^^ { v => CharTok(v) })     |
    (quotedString ^^ { v => StringTok(v) })   |
    (symbol       ^^ { v => SymbolTok(v) })   |
    (reserved     ^^ { v => ReservedTok(v) })
  }

  def test[T](input: String, parser: Parser[T]): T = {
    val reader = new CharArrayReader(input.toArray)
    phrase(parser)(reader).get
  }

  def test(input: String): Token = test(input, token)
}