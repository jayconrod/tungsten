package tungsten

import java.io._
import Utilities._

object Assembler {
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
        val outputFile = fileWithExtension(inputFile, ".w", ".wo")
        writeModuleToFile(module, outputFile)
      }
    }

    if (errorOccurred)
      System.exit(FAILURE_CODE)
  }

  def readModuleFromFile(file: File): Option[Module] = {
    try {
      ModuleIO.readText(file) match {
        case Left(module) => Some(module)
        case Right(errors) => {
          errors.foreach(System.err.println _)
          errorOccurred = true
          None
        }
      }
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
      val reader = new InputStreamReader(System.in)
      ModuleIO.readText(reader) match {
        case Left(module) => module
        case Right(errors) => {
          System.err.println("errors while reading <stdin>:")
          errors.foreach(System.err.println _)
          System.exit(1)
          null
        }
      }
    } catch {
      case exn: IOException => {
        exitWithFailure("<stdin>: IO error: " + exn.getMessage)
        null
      }
    }
  }

  def writeModuleToFile(module: Module, file: File) {
    try {
      ModuleIO.writeBinary(module, file)
    } catch {
      case exn: IOException => {
        System.err.println(file + ": IO error: " + exn.getMessage)
        errorOccurred = true
      }
    }
  }

  def writeModuleToStdout(module: Module) {
    try {
      ModuleIO.writeBinary(module, System.out)
    } catch {
      case exn: IOException => {
        exitWithFailure("<stdout>: IO error: " + exn.getMessage)
      }
    }
  }
}
