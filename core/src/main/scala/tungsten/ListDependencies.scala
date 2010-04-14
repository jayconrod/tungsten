package tungsten

import java.io._
import Utilities._

object ListDependencies {
  def usage {
    System.err.println("ListDependencies file\n")
  }

  def main(args: Array[String]) {
    if (args.size != 1) {
      usage
      System.exit(ERROR_CODE)
    }

    val inputFile = new File(args(0))
    val module = try {
      ModuleIO.readBinary(inputFile)
    } catch {
      case exn: IOException => {
        exitWithFailure(inputFile + ": error: " + exn.getMessage)
        null
      }
    }

    listDependencies(module, inputFile.getParentFile, Nil) foreach { dep =>
      val (name, file) = dep
      val filename = file.map(_.toString).getOrElse("<missing>")
      System.err.print("%s => %s\n".format(name, filename))
    }
  }

  def listDependencies(module: Module,
                       directory: File,
                       found: List[(Symbol, Option[File])]): List[(Symbol, Option[File])] =
  {
    (found /: module.dependencies) { (found, dep) =>
      if (found.exists(_._2 == dep.name))
        found
      else {
        try {
          val file = Loader.findModuleFile(dep, directory, module.searchPaths)
          val library = ModuleIO.readBinary(file)
          listDependencies(library, file.getParentFile, (dep.name, Some(file)) :: found)
        } catch {
          case exn: IOException => (dep.name, None) :: found
        }
      }
    }
  }
}
