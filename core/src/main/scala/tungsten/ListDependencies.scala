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
