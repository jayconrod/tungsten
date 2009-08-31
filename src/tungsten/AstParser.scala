package tungsten

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object AstParser extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters ++= List(":")
  lexical.reserved ++= List("#global", "#unit")

  def symbol = ident ^^ { new Symbol(_) }

  def ty = "#unit" ^^^ AstUnitType(Nowhere)

  def global = {
    "#global" ^^^ AstGlobal(new Symbol("foo"), AstUnitType(Nowhere), None, Nowhere)
  }

  def definition = global

  def module = rep(definition) ^^ { AstModule(_) }

  def parse(input: String) = {
    val tokens = new lexical.Scanner(input)
    phrase(module)(tokens)
  }
}
