package tungsten.llvm

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

object Parser extends Parsers with ImplicitConversions {
  type Elem = AnyRef

  implicit def reserved(r: String): Parser[String] = elem(ReservedToken(r)) ^^^ r

  def string: Parser[String] = {
    accept("string", { case t: StringToken => t.value })
  }

  def localSymbol: Parser[String] = {
    accept("local symbol", { case t: SymbolToken if !t.isGlobal => t.value })
  }

  def globalSymbol: Parser[String] = {
    accept("global symbol", { case t: SymbolToken if t.isGlobal => t.value })
  }    

  def targetDataLayout: Parser[Option[String]] = {
    opt("target" ~> "datalayout" ~> "=" ~> string)
  }

  def targetTriple: Parser[Option[String]] = {
    opt("target" ~> "triple" ~> "=" ~> string)
  }

  def module: Parser[Module] = {
    targetDataLayout ~ targetTriple ^^ {
      case dl ~ t => new Module(dl, t, Map[String, Definition]())
    }
  }

  def test(input: String) = {
    val reader = new Lexer.Scanner(input)
    val result = phrase(module)(reader)
    result match {
      case Success(ast, _) => ast
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}