package tungsten

import scala.util.matching.Regex
import java.io._
import Utilities._

final object Loader {
  def loadAndLinkProgram(file: File) = {
    val modules = loadModuleAndDependencies(file, ModuleType.PROGRAM, Version.MIN, Version.MAX)
    
    val program = Linker.linkModules(modules,
                                     "default",
                                     ModuleType.PROGRAM,
                                     Version.MIN,
                                     Nil,
                                     Nil)
    val errors = program.validateProgram
    if (!errors.isEmpty) {
      throw new IOException("validation errors in program %s:\n%s".
                              format(file, errors.mkString("\n")))
    }
    program
  }

  def loadModuleAndDependencies(file: File,
                                ty: ModuleType,
                                minVersion: Version,
                                maxVersion: Version): List[Module] =
  {
    val module = ModuleIO.readBinary(file)
    if (module.ty != ty)
      throw new IOException("file %s was supposed to be a %s module".format(file.getName, ty))
    if (module.version < minVersion) {
      throw new IOException("library %s has version %s which is less than the minimum, %s".
                          format(file, module.version, minVersion))
    }
    if (module.version > maxVersion) {
      throw new IOException("library %s has version %s which is greater than the maximum, %s".
                          format(file, module.version, maxVersion))
    }
    val errors = module.validate
    if (!errors.isEmpty) {
      throw new IOException("validation errors in module %s:\n%s".
                          format(file, errors.mkString("\n")))
    }

    val libraries = module.dependencies.flatMap { dependency =>
      val libraryFile = findModuleFile(dependency, module.searchPaths)
      loadModuleAndDependencies(libraryFile, 
                                ModuleType.LIBRARY, 
                                dependency.minVersion,
                                dependency.maxVersion)
    }
    module :: libraries
  }

  def findModuleFile(dependency: ModuleDependency, 
                     searchPaths: List[File]): File =
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
      case None => throw new IOException("could not find library: " + dependency)
    }
  }

  val LibraryName = new Regex("([A-Za-z0-9_$.]+)-([0-9.]+).wl")
}
