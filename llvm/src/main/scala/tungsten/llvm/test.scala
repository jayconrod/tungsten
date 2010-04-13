package tungsten.llvm

import java.io._
import scala.util.parsing.input.CharSequenceReader
import tungsten.Utilities._

object test {
  def main(args: Array[String]) {
    if (args.size != 1) {
      System.err.println("usage: test file.ll")
      System.exit(1)
    }

    val filename = args(0)
    val file = new File(filename)
    val text = readContentsOfFile(file)
    val reader = new CharSequenceReader(text)
    val scanner = new Lexer.Scanner(reader)
    val module = Parser.phrase(Parser.module)(scanner) match {
      case Parser.Success(ast, _) => Left(ast)
      case parseError: Parser.NoSuccess => {
        System.err.println("parse error: " + parseError.msg)
        System.exit(1)
        null
      }
    }
    System.out.println(module)
  }
}
