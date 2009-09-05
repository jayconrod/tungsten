package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._

object AstLexer extends Parsers {
  type Elem = Char

  private val reservedStrings = Set("()")

  def whitespaceChar = elem(' ') | elem('\n') | elem('\t')

  def whitespace = rep(whitespaceChar)

  def errorToken(msg: String, loc: Location) = {
    val e = ErrorToken(msg)
    e.location = loc
    e
  }

  def reserved: Parser[Token] = {
/*    def parseReserved(r: String): Parser[Token] = {
      accept(r.toList) ^^ { s => ReservedToken(s.mkString) }
    }
    val reservedArray = new Array[String](reservedStrings.size)
    reservedStrings.copyToArray(reservedArray, 0)
    scala.util.Sorting.quickSort(reservedArray)
    val reservedParsers = reservedArray.toList.map(parseReserved)
    val fail: Parser[Token] = failure("no matching reserved string")
    reservedParsers.foldRight(fail)((x: Parser[Token], y: Parser[Token]) => y | x)
*/
    accept("()".toList) ^^ { s => ReservedToken("()") }
  }

  def token: Parser[Token] = {
    reserved | failure("illegal character")
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
