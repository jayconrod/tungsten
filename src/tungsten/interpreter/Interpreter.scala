package tungsten.interpreter

import java.io._
import scala.util.matching.Regex
import tungsten._
import tungsten.Utilities._

object Interpreter {
  def usage = System.err.println("usage: Interpreter program\n")

  def main(args: Array[String]) = {
    if (args.size != 1) {
      usage
      System.exit(ERROR_CODE)
    }

    var program: Module = null
    try {
      val file = new File(args(0))
      val modules = load(file, ModuleType.PROGRAM, Version.MIN, Version.MAX)
      program = Linker.linkModules(modules,
                                   "default",
                                   ModuleType.PROGRAM,
                                   Version.MIN,
                                   Nil,
                                   Nil)
      val errors = program.validate
      if (!errors.isEmpty) {
        errors.foreach(System.err.println(_))
        System.exit(FAILURE_CODE)
      }
    } catch {
      case exn: IOException => exitWithFailure(exn.getMessage)
    }
    assert(program != null)

    val env = new Environment(program)
    val errorCode = env.run
    System.exit(errorCode)
  }

  def load(file: File, 
           ty: ModuleType,
           minVersion: Version,
           maxVersion: Version): List[Module] = 
  {
    val module = ModuleIO.readBinary(file)
    if (module.ty != ty)
      exitWithFailure("file %s was expected to be a %s module".format(file.getName, ty))
    if (module.version < minVersion) {
      exitWithFailure("library %s has version %s which is less than the minimum, %s".
                        format(file, module.version, minVersion))
    }
    if (module.version > maxVersion) {
      exitWithFailure("library %s has version %s which is greater than the maximum, %s".
                        format(file, module.version, maxVersion))
    }

    val libraries = module.dependencies.flatMap { dependency =>
      val libraryFile = findModule(dependency, module.searchPaths)
      load(libraryFile, ModuleType.LIBRARY, dependency.minVersion, dependency.maxVersion)
    }
    module :: libraries
  }

  def findModule(dependency: ModuleDependency, searchPaths: List[File]): File =
  {
    val files = for (dir  <- searchPaths.iterator;
                     file <- dir.listFiles.iterator)
                  yield file

    def isMatchingLibrary(file: File): Boolean = {
      file.getName match {
        case LibraryName(nameStr, versionStr) => {
          try {
            val libName = symbolFromString(nameStr)
            val libVersion = parseVersion(versionStr)
            libName == dependency.name && 
              dependency.minVersion <= libVersion && libVersion <= dependency.maxVersion
          } catch {
            case _ => false
          }
        }
        case _ => false
      }
    }

    files.find(isMatchingLibrary _) match {
      case Some(file) => file
      case None => {
        exitWithFailure("could not find library: " + dependency)
        null
      }
    }
  }

  val LibraryName = new Regex("([A-Za-z0-9_$.]+)-([0-9.]+).wl")
}
