package tungsten.interpreter

import java.io._
import tungsten._
import tungsten.Utilities._

object Interpreter {
  def usage = System.err.println("usage: Interpreter objectfile\n")

  def main(args: Array[String]) = {
    if (args.size != 1) {
      usage
      System.exit(ERROR_CODE)
    }

    var module: Module = null
    try {
      val file = new File(args(0))
      module = ModuleIO.readBinary(file)
      val errors = module.validate
      if (!errors.isEmpty) {
        System.err.println("Invalid module:")
        errors.foreach(System.err.println(_))
        System.exit(FAILURE_CODE)
      }
    } catch {
      case exn: IOException => {
        System.err.println("error: " + exn.getMessage)
        System.exit(FAILURE_CODE)
      }
    }
    
    val env = new Environment(module)
    val errorCode = env.run
    System.exit(errorCode)
  }
}
