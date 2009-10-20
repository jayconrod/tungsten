package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._

object AstLexer extends Parsers {
  type Elem = Char

  val reservedStrings = Set("()", ":", ",", "=", "{", "}", "(", ")", "[", "]", "<:", ">:",
    "#global", "#block", "#function", "#field", "#struct", "#class", "#fields", "#methods",
      "#interface",
    "#unit", "#int8", "#int16", "#int32", "#int64", 
    "#return", "#branch", "#icall", "#scall") 

  def chrExcept(cs: Char*) = {
    elem("", c => cs.forall(c != _))
  }

  def whitespaceChar = elem(' ') | elem('\n') | elem('\t')

  def whitespace = rep(whitespaceChar)

  def errorToken(msg: String, loc: Location) = {
    val e = ErrorToken(msg)
    e.location = loc
    e
  }

  def reserved: Parser[Token] = {
    def parseReserved(r: String): Parser[Token] = {
      accept(r.toList) ^^ { s => ReservedToken(s.mkString) }
    }
    val reservedArray = new Array[String](reservedStrings.size)
    reservedStrings.copyToArray(reservedArray, 0)
    scala.util.Sorting.quickSort(reservedArray)
    val reservedParsers = reservedArray.toList.map(parseReserved)
    val fail: Parser[Token] = failure("no matching reserved string")
    reservedParsers.foldRight(fail)((x: Parser[Token], y: Parser[Token]) => y | x)
  }

  def letter = elem("letter", _.isLetter)

  def digit = elem("digit", _.isDigit)
  def numChars: Parser[Long] = {
    opt('-') ~ rep1(digit) ^^ { 
      case None ~ digits => digits.mkString.toLong
      case Some(_) ~ digits => -digits.mkString.toLong
    }
  }

  def identifierChar: Parser[Char] = letter | digit | elem('_')

  def identifier: Parser[String] = {
    (letter | elem('_')) ~ rep(identifierChar) ^^ { 
      case first ~ rest => (first :: rest).mkString
    }
  }

  def byte: Parser[Byte] = numChars <~ 'b' ^^ { _.asInstanceOf[Byte] }
  def short: Parser[Short] = numChars <~ 's' ^^ { _.asInstanceOf[Short] }
  def int: Parser[Int] = numChars ^^ { _.asInstanceOf[Int] }
  def long: Parser[Long] = numChars <~ 'L'

  def symbol: Parser[Symbol] = {
    val idNum: Parser[Int] = {
      opt(elem('#') ~> int) ^^ {
        case Some(i) => i
        case None => 0
      }
    }
    rep1sep(identifier, elem('.')) ~ idNum ^^ {
      case name ~ id => Symbol(name, id)
    }
  }

  def location: Parser[Location] = {
    elem('<') ~ rep1(chrExcept(':')) ~ elem(':') ~
      int ~ elem('.') ~ int ~ elem('-') ~ int ~ elem('.') ~ int ~ elem('>') ^^ {
      case _ ~ filename ~ _ ~ beginLine ~ _ ~ beginColumn ~ _ ~ endLine ~ _ ~ endColumn ~ _ =>
        Location(filename.mkString, beginLine, beginColumn, endLine, endColumn)
    }
  }

  def token: Parser[Token] = {
    reserved | 
    (symbol ^^ { SymbolToken(_) }) | 
    (location ^^ { LocationToken(_) }) | 
    (byte ^^ { ByteToken(_) }) |
    (short ^^ { ShortToken(_) }) |
    (long ^^ { LongToken(_) }) |
    (int ^^ { IntToken(_) }) |      // must follow others since it doesn't have a suffix
    failure("illegal character")
  }

  def test(in: String): Token = {
    val reader = new Scanner(in)
    val tok = reader.first
    if (reader.rest.atEnd) tok else ErrorToken("did not match full input")
  }

  class Scanner(filename: String, in: Reader[Elem]) extends Reader[Token] {
    def this(in: String) = this("<UNKNOWN>", new CharArrayReader(in.toCharArray))

    implicit private def locationFromPosition(pos: Position): Location = {
      new Location(filename, pos.line, pos.column, pos.line, pos.column)
    }

    private val (tok, rest1, rest2) = whitespace(in) match {
      case Success(_, in1) => {
        token(in1) match {
          case Success(tok, in2) => {
            val endLoc = new Location(filename, in2.pos.line, in2.pos.column - 1)
            tok.location = in1.pos combine endLoc
            (tok, in1, in2)
          }
          case ns: NoSuccess => {
            (errorToken(ns.msg, ns.next.pos), ns.next, skip(ns.next))
          }
        }
      }
      case ns: NoSuccess => (errorToken(ns.msg, ns.next.pos), ns.next, skip(ns.next))
    }
    private def skip(in: Reader[Elem]) = if (in.atEnd) in else in.rest

    override def source = in.source
    override def offset = in.offset
    def first = tok
    def rest = new Scanner(filename, rest2)
    def pos = rest1.pos
    def atEnd = {
      if (in.atEnd)
        true
      else {
        whitespace(in) match {
          case Success(_, in1) => in1.atEnd
          case _ => false
        }
      }
    }
  }
}
