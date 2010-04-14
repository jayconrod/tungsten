package tungsten.llvm

import org.junit.Test
import org.junit.Assert._

class ParserTest {
  def test[T](input: String, parser: Parser.Parser[T], expected: T) {
    val scanner = new Lexer.Scanner(input)
    val result = Parser.phrase(parser)(scanner)
    result match {
      case Parser.Success(ast, _) => assertEquals(expected, ast)
      case parseError: Parser.NoSuccess => fail(parseError.msg)
    }
  }

  def testModule(input: String, expected: Module) {
    test(input, Parser.module, expected)
  }

  @Test
  def empty {
    testModule("", new Module(None, None, Map[String, Definition]()))
  }

  @Test
  def whitespace {
    testModule(" \t\n", new Module(None, None, Map[String, Definition]()))
  }

  @Test
  def headers {
    val dataLayoutString = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128"
    val tripleString = "x86_64-linux-gnu"
    testModule("; ModuleID = '<stdin>'\n" +
               "target datalayout = \"" + dataLayoutString + "\"\n" +
               "target triple = \"" + tripleString + "\"\n",
               new Module(Some(dataLayoutString), Some(tripleString), Map[String, Definition]()))
  }

  @Test
  def localSymbol {
    val input = "%test"
    test(input, Parser.localSymbol, input)
  }

  @Test
  def globalSymbol {
    val input = "@test"
    test(input, Parser.globalSymbol, input)
  }
}
