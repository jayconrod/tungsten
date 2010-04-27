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

  lazy val ty: Parser[Type] = {
    def makePointerType(elementType: Type, count: Int): Type = {
      if (count == 0)
        elementType
      else
        makePointerType(PointerType(elementType), count - 1)
    }
    def arrayTy: Parser[Type] = {
      def arraySize = {
        ("?" ^^^ None) |
        (integer ^^ { case i => Some(i.toInt) })
      }
      "[" ~> (arraySize <~ "x") ~ ty <~ "]" ^^ { case s ~ ety => ArrayType(s, ety) }
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
      ("struct" ~> symbol ^^ { case name => StructType(name) }) |
      arrayTy 
    }
                  
    basicTy ~ rep("*") ^^ { case ety ~ stars => makePointerType(ety, stars.size) }    
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
