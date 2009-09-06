package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

object AstParser extends Parsers {
  type Elem = Token

  def symbol: Parser[Symbol] = {
    elem("symbol", _.isInstanceOf[SymbolToken]) ^^ { 
      case SymbolToken(sym) => sym
      case _ => throw new AssertionError
    }
  }

  def ty: Parser[AstType] = {
    "#unit" ^^^ AstUnitType()
  }

  def global: Parser[AstGlobal] = {
    "#global" ~ symbol ~ ":" ~ ty ^^ { case _ ~ name ~ _ ~ t => AstGlobal(name, t, None) }
  }

  def definition: Parser[AstDefinition] = global

  def module: Parser[AstModule] = rep(definition) ^^ { AstModule(_) }

  implicit def reserved(r: String): Parser[Token] = elem(ReservedToken(r))

  def test(input: String) = {
    val reader = new AstLexer.Scanner(input)
    val result = phrase(module)(reader)
    result.get
  }
}
