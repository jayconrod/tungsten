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

  lazy val value: Parser[Value] = {
    def arrayValue: Parser[Value] = {
      arrayTy ~ ("{" ~> repsep(value, ",") <~ "}") ^? { 
        case ArrayType(Some(size), elementType) ~ es if size == es.size => {
          ArrayValue(elementType, es)
        }
      }
    }
    def structValue: Parser[Value] = {
      structTy ~ ("{" ~> repsep(value, ",") <~ "}") ^? {
        case StructType(name) ~ es => StructValue(name, es)
      }
    }

    ("(" ~ ")"           ^^^ UnitValue)                  |
    ("true"              ^^^ BooleanValue(true))         |
    ("false"             ^^^ BooleanValue(false))        |
    (char                 ^^ { v => CharValue(v) })      |
    (string               ^^ { v => StringValue(v) })    |
    ("int8" ~> integer    ^^ { v => IntValue(v, 8) })    |
    ("int16" ~> integer   ^^ { v => IntValue(v, 16) })   |
    ("int32" ~> integer   ^^ { v => IntValue(v, 32) })   |
    ("int64" ~> integer   ^^ { v => IntValue(v, 64) })   |
    ("float32" ~> float   ^^ { v => FloatValue(v, 32) }) |
    ("float64" ~> float   ^^ { v => FloatValue(v, 64) }) |
    ("null"              ^^^ NullValue)                  |
    arrayValue                                           |
    structValue                                          |
    (ty ~ symbol          ^^ { case t ~ n => DefinedValue(n, t) })
  }

  lazy val ty: Parser[Type] = {
    def makePointerType(elementType: Type, count: Int): Type = {
      if (count == 0)
        elementType
      else
        makePointerType(PointerType(elementType), count - 1)
    }
    def basicTy: Parser[Type] = {
      ("unit"     ^^^ UnitType)      |
      ("boolean"  ^^^ BooleanType)   |
      ("char"     ^^^ CharType)      |
      ("string"   ^^^ StringType)    |
      ("int8"     ^^^ IntType(8))    |
      ("int16"    ^^^ IntType(16))   |
      ("int32"    ^^^ IntType(32))   |
      ("int64"    ^^^ IntType(64))   |
      ("float32"  ^^^ FloatType(32)) |
      ("float64"  ^^^ FloatType(64)) |
      ("nulltype" ^^^ NullType)      |
      structTy                       |
      arrayTy 
    }
                  
    basicTy ~ rep("*") ^^ { case ety ~ stars => makePointerType(ety, stars.size) }    
  }

  lazy val arrayTy: Parser[ArrayType] = {
    def arraySize = {
      ("?" ^^^ None) |
      (integer ^^ { case i => Some(i.toInt) })
    }
    "[" ~> (arraySize <~ "x") ~ ty <~ "]" ^^ { case s ~ ety => ArrayType(s, ety) }
  }

  lazy val structTy: Parser[StructType] = {
    "struct" ~> symbol ^^ { case name => StructType(name) }
  }

  implicit def reserved(r: String): Parser[String] = {
    assert(Lexer.reservedStrings(r))
    elem(ReservedTok(r)) ^^^ r
  }
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
