package tungsten

import java.io._
import Utilities._

object Disassembler {
  var errorOccurred = false

  def main(args: Array[String]) = {
    if (args.isEmpty) {
      val module = readModuleFromStdin
      writeModuleToStdout(module)
    } else {
      for (filename <- args;
           val inputFile = new File(filename);
           module <- readModuleFromFile(inputFile))
      {
        val outputFile = fileWithExtension(inputFile, ".wo", ".w")
        writeModuleToFile(module, outputFile)
      }
    }

    if (errorOccurred)
      System.exit(FAILURE_CODE)
  }

  def readModuleFromFile(file: File): Option[Module] = {
    try {
      Some(ModuleIO.readBinary(file))
    } catch {
      case exn: IOException => {
        System.err.println(file + ": IO error: " + exn.getMessage)
        errorOccurred = true
        None
      }
    }
  }

  def readModuleFromStdin: Module = {
    try {
      ModuleIO.readBinary(System.in)
    } catch {
      case exn: IOException => {
        exitWithFailure("<stdin>: IO error: " + exn)
        null
      }
    }
  }

  def writeModuleToFile(module: Module, file: File) {
    try {
      ModuleIO.writeText(module, file)
    } catch {
      case exn: IOException => {
        System.err.println(file + ": IO error: " + exn.getMessage)
        errorOccurred = true
      }
    }
  }

  def writeModuleToStdout(module: Module) {
    val writer = new OutputStreamWriter(System.out)
    try {
      ModuleIO.writeText(module, writer)
    } catch {
      case exn: IOException => {
        exitWithFailure("<stdout>: IO error: " + exn.getMessage)
      }
    }
  }
}
