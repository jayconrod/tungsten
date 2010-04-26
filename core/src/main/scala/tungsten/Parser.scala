package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader
import java.io.File

object Parser extends Parsers with ImplicitConversions {
  type Elem = Lexer.Token

  def module(file: Option[File]): Parser[Module] = {
    failure("not implemented")
  }

  implicit def reserved(r: String): Parser[String] = elem(ReservedTok(r)) ^^^ r
  lazy val symbol: Parser[Symbol] = accept("symbol", { case SymbolTok(v) => v })
  lazy val integer: Parser[Long] = accept("integer", { case IntTok(v) => v })
  lazy val float: Parser[Double] = accept("float", { case FloatTok(v) => v })
  lazy val char: Parser[Char] = accept("char", { case CharTok(v) => v })
  lazy val string: Parser[String] = accept("string", { case StringTok(v) => v })

  def test[T](input: String, parser: Parser[T]): T = {
    val reader = new Lexer.Scanner(input)
    phrase(parser)(reader) match {
      case Success(r, _) => r
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }

  def test(input: String): Module = {
    test(input, module(None))
  }
}
