package tungsten.interpreter

import java.io._
import java.nio.channels.FileChannel.MapMode._
import scala.util.parsing.input.CharSequenceReader
import tungsten._
import tungsten.Utilities._

object Interpreter {
  def main(args: Array[String]) = {
    if (args.size != 1)
      throw new IllegalArgumentException
    val filename = args(0)
    val file = new File(filename)
    val text = readFile(file)
    val reader = new CharSequenceReader(text)
    val scanner = new AstLexer.Scanner(filename, reader)
    AstParser.phrase(AstParser.module)(scanner) match {
      case AstParser.Success(ast, _) => {
        val module = ast.compile
        val env = new Environment(module)
        env.run
        System.exit(env.returnCode)
      }
      case error: AstParser.NoSuccess => {
        System.err.println(error.msg)
        System.exit(1)
      }
    }
  }
}
