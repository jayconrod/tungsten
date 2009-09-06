package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

object AstParser extends Parsers with ImplicitConversions {
  type Elem = Token

  def symbol: Parser[Symbol] = {
    elem("symbol", _.isInstanceOf[SymbolToken]) ^^ { 
      case SymbolToken(sym) => sym
      case _ => throw new AssertionError
    }
  }

  def location: Parser[Location] = {
    val locParser = elem("location", _.isInstanceOf[LocationToken]) ^^ {
      case LocationToken(loc) => loc
      case _ => throw new AssertionError
    }
    opt(locParser) ^^ {
      case Some(l) => l
      case None => Nowhere
    }
  }

  def ty: Parser[AstType] = {
    ("#unit" ~> location ^^ { AstUnitType(_) }) |
    ("#int8" ~> location ^^ { AstIntType(8, _) }) |
    ("#int16" ~> location ^^ { AstIntType(16, _) }) |
    ("#int32" ~> location ^^ { AstIntType(32, _) }) |
    ("#int16" ~> location ^^ { AstIntType(64, _) })
  }

  def value: Parser[AstValue] = {
    val integer = elem("integer", _.isInstanceOf[IntegerToken]) ^^ {
      case IntegerToken(i) => i
      case _ => throw new AssertionError
    }
    ("()" ~> location ^^ { AstUnitValue(_) }) |
    (integer ~ location ^^ { flatten2(AstIntValue(_, _)) })
  }

  def global: Parser[AstGlobal] = {
    "#global" ~ location ~ symbol ~ ":" ~ ty ~ opt("=" ~> value) ^^ { 
      case _ ~ loc ~ name ~ _ ~ t ~ iv => AstGlobal(name, t, iv, loc)
    }
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
