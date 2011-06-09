/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

import scala.util.matching.Regex
import java.io._
import Utilities._

final object Loader {
  def loadAndLinkProgram(file: File) = {
    val modules = loadModuleAndDependencies(file, ModuleType.PROGRAM, 
                                            Version.MIN, Version.MAX, Nil)
    
    val program = Linker.linkModules(modules,
                                     "default",
                                     ModuleType.PROGRAM,
                                     Version.MIN,
                                     None,
                                     Nil,
                                     Nil)
    val errors = program.validateProgram
    if (!errors.isEmpty) {
      throw new IOException("validation errors in program %s:\n%s".
                              format(file, errors.mkString("\n")))
    }
    program
  }

  def loadDependenciesForModule(module: Module,
                                directory: File,
                                alreadyLoaded: List[Module]): List[Module] =
  {
    assert(alreadyLoaded.contains(module))
    (alreadyLoaded /: module.dependencies) { (loaded, dependency) =>
      if (loaded.exists(_.name == dependency.name))
        loaded
      else {
        val libraryFile = findModuleFile(dependency, directory, module.searchPaths)
        loadModuleAndDependencies(libraryFile,
                                  ModuleType.LIBRARY,
                                  dependency.minVersion,
                                  dependency.maxVersion,
                                  loaded)
      }
    }
  }

  def loadModuleAndDependencies(file: File,
                                ty: ModuleType,
                                minVersion: Version,
                                maxVersion: Version,
                                alreadyLoaded: List[Module]): List[Module] =
  {
    val module = ModuleIO.readBinary(file)
    if (alreadyLoaded.exists(_.name == module.name))
      alreadyLoaded
    else {    
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
      loadDependenciesForModule(module, file.getParentFile, module :: alreadyLoaded)
    }
  }

  def findModuleFile(dependency: ModuleDependency, 
                     directory: File,
                     searchPaths: List[File]): File =
  {
    val files = searchPaths.view.flatMap { path =>
      val dir = if (!path.isAbsolute)
        new File(directory, path.toString)
      else
        path
      val contents = dir.listFiles
      if (contents != null)
        contents.view
      else
        Nil.view
    }

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
