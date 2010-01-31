package tungsten.interpreter

import java.io._
import tungsten._
import Utilities._

object Interpreter {
  def usage = System.err.println("usage: Interpreter program\n")

  def main(args: Array[String]) = {
    if (args.size != 1) {
      usage
      System.exit(ERROR_CODE)
    }

    var program: Module = null
    try {
      program = Loader.loadAndLinkProgram(new File(args(0)))
    } catch {
      case exn: IOException => exitWithFailure(exn.getMessage)
    }
    assert(program != null)
     
    val env = new Environment(program)
    val errorCode = env.run
    System.exit(errorCode)
  }
}
