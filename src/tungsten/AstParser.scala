package tungsten

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object AstParser extends StandardTokenParsers {
  lexical.delimiters ++= Nil

  def module = success(AstModule(Nil))

  def parse(input: String) = {
    val tokens = new lexical.Scanner(input)
    phrase(module)(tokens)
  }
}
