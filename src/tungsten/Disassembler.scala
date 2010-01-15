package tungsten

import java.io._
import Utilities._

object Disassembler {
  def usage = System.err.println("usage: Disassembler object1 object2 ...\n")

  def main(args: Array[String]) = {
    if (args.size == 0) {
      usage
      System.exit(ERROR_CODE)
    }

    var error = false
    for (filename <- args) {
      try {
        val inputFile = new File(filename)
        val module = ModuleIO.readBinary(inputFile)
        val outputFile = fileWithExtension(inputFile, ".wo", ".w")
        ModuleIO.writeText(module, outputFile)
      } catch {
        case exn: IOException => {
          System.err.println(filename + ": error: " + exn)
          error = true
        }
      }
    }

    System.exit(if (error) FAILURE_CODE else 0)
  }
}
