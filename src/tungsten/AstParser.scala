package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader

object AstParser extends Parsers {
  type Elem = Token

  def module: Parser[AstModule] = success(AstModule(Nil))

  def test(input: String) = {
    val reader = new AstLexer.Scanner(input)
    val result = phrase(module)(reader)
    result.get
  }
}
