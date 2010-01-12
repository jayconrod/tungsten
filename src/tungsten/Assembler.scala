package tungsten

import java.io._
import Utilities._

object Assembler {
  def usage = System.err.println("usage: Assembler source1 source2...\n")

  def main(args: Array[String]) = {
    if (args.size == 0) {
      usage
      System.exit(ERROR_CODE)
    }

    for (filename <- args) {
      try {
        val inputFile = new File(filename)
        ModuleIO.readText(inputFile) match {
          case Right(errors) => {
            errors.foreach(System.err.println(_))
            System.exit(1)
          }
          case Left(module) => {
            val outputFile = fileWithExtension(inputFile, ".w", ".wo")
            ModuleIO.writeBinary(module, outputFile)
          }
        }
      } catch {
        case exn: IOException => {
          System.err.println("IO exception: " + exn)
          System.exit(1)
        }
      }
    }
  }
}
