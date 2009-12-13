package tungsten.interpreter

import java.io._
import java.nio.channels.FileChannel.MapMode._
import scala.util.parsing.input.CharSequenceReader
import tungsten._
import tungsten.Utilities._

object Interpreter {
  val ERROR_CODE = 127

  def usage = System.err.println("usage: Interpreter sourcefile\n")

  def main(args: Array[String]) = {
    def getFilename = {
      if (args.size != 1) {
        usage
        None
      } else
        Some(args(0))
    }

    def parse(filename: String) = {
      val file = new File(filename)
      val text = readFile(file)
      val reader = new CharSequenceReader(text)
      val scanner = new AstLexer.Scanner(filename, reader)
      AstParser.phrase(AstParser.module)(scanner) match {
        case AstParser.Success(ast, _) => Some(ast)
        case parseError: AstParser.NoSuccess => {
          System.err.println(parseError.msg)
          None
        }
      }
    }
    
    def compile(ast: AstModule) = {
      ast.compile match {
        case Left(module) => Some(module)
        case Right(errors) => {
          errors.foreach(System.err.println(_))
          None
        }
      }
    }

    def validate(module: Module) = {
      val errors = module.validate
      errors.foreach(System.err.println(_))
      errors.isEmpty
    }

    val returnCode = getFilename flatMap { filename =>
      parse(filename) flatMap { ast =>
        compile(ast) flatMap { module =>
          if (validate(module)) {
            val env = new Environment(module)
            Some(env.run)
          } else
            None
        }
      }
    }

    System.exit(returnCode.getOrElse(ERROR_CODE))
  }
}
