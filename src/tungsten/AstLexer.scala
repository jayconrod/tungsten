package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._

object AstLexer extends Parsers {
  type Elem = Char

  def whitespaceChar = elem(' ') | elem('\n') | elem('\t')

  def whitespace = rep(whitespaceChar)

  def errorToken(msg: String) = ErrorToken(msg)

  def token: Parser[Token] = failure("illegal character")

  class Scanner(in: Reader[Elem]) extends Reader[Token] {
    def this(in: String) = this(new CharArrayReader(in.toCharArray))

    private val (tok, rest1, rest2) = whitespace(in) match {
      case Success(_, in1) => {
        token(in1) match {
          case Success(tok, in2) => (tok, in1, in2)
          case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
        }
      }
      case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
    }
    private def skip(in: Reader[Elem]) = if (in.atEnd) in else in.rest

    override def source = in.source
    override def offset = in.offset
    def first = tok
    def rest = new Scanner(rest2)
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
