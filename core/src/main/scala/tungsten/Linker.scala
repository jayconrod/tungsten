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
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.collection.immutable.ListSet
import java.io._
import Utilities._

object Linker {
  def usage {
    println("Linker [options] inputfiles... [-o outputfile]\n" +
            "Options:\n" +
            "  -n name                          module name\n" +
            "  -t program|library|intermediate  module type\n" +
            "  -v version                       module version\n" +
            "  -o output                        output file\n" +
            "  -L paths                         add library search paths\n" +
            "\n" +
            "Input files can be:\n" +
            "  filename.wo                      explicit filename of module\n" +
            "  -lname[:minversion-maxversion]   symbolic name of library (added to dependencies)\n")
  }

  var moduleName = Symbol("default")
  var moduleTy = ModuleType.INTERMEDIATE
  var moduleVersion = Version.MIN
  val moduleDependencies = new ArrayBuffer[ModuleDependency]
  val moduleSearchPaths = new ArrayBuffer[File]

  var moduleOutputFile: Option[File] = None

  val inputFiles = new ArrayBuffer[File]

  def main(args: Array[String]) {
    parseArguments(args)
    val outputFile = moduleOutputFile.getOrElse(defaultOutputFile)

    val inputModules = loadModules
    val outputModule = linkModules(inputModules,
                                   moduleName,
                                   moduleTy,
                                   moduleVersion,
                                   Some(outputFile),
                                   moduleDependencies.toList,
                                   moduleSearchPaths.toList)
    validateOutput(outputModule, outputFile.getParentFile)
      
    try { 
      storeModule(outputModule, outputFile)
    } catch {
      case exn: IOException => exitWithFailure("I/O error while storing module: " + exn.getMessage)
    }     
  }

  def parseArguments(args: Array[String]) {
    var i = 0
    def next: String = {
      if (i + 1 < args.size) {
        i += 1
        args(i)
      } else {
        exitWithError("parameter " + args(i) + " requires an argument")
        ""
      }
    }

    while (i < args.size) {
      args(i) match {
        case "-h" => {
          usage
          System.exit(0)
        }
        case "-n" => {
          val nameStr = next
          try {
            moduleName = symbolFromString(nameStr)
          } catch {
            case _ => exitWithError("Invalid module name: " + nameStr)
          }
        }
        case "-t" => {
          import ModuleType._
          next match {
            case "program" => moduleTy = PROGRAM
            case "library" => moduleTy = LIBRARY
            case "intermediate" => moduleTy = INTERMEDIATE
            case _ => exitWithError("Invalid type. Must be program, library, or intermediate.")
          }
        }
        case "-v" => {
          Utilities.tryParseVersion(next) match {
            case Some(v) => moduleVersion = v
            case None => exitWithError("Invalid module version")
          }
        }
        case "-o" => moduleOutputFile = Some(new File(next))
        case "-L" => {
          moduleSearchPaths ++= next.split(File.pathSeparator).map(new File(_))
        }
        case LibraryArgument(nameStr, minVersionStr, maxVersionStr) => {
          try {
            val name = symbolFromString(nameStr)
            val minVersion = parseVersion(minVersionStr, Version.MIN)
            val maxVersion = parseVersion(maxVersionStr, Version.MAX)
            moduleDependencies += new ModuleDependency(name, minVersion, maxVersion)
          } catch {
            case _ => exitWithError("Invalid library: " + args(i))
          }
        }
        case _ if args(i).startsWith("-") => {
          exitWithError("Invalid option: " + args(i))
        }
        case _ => inputFiles += new File(args(i))
      }
      i += 1
    }

    if (moduleTy == ModuleType.LIBRARY && moduleVersion == Version.MIN)
      exitWithError("Libraries must have a version. Use -v to set the version.")
  }

  def parseVersion(versionStr: String, defaultVersion: Version): Version = {
    if (versionStr == null || versionStr.isEmpty)
      defaultVersion
    else
      Utilities.parseVersion(versionStr)
  }

  val LibraryArgument = new Regex("-l([A-Za-z0-9_$.]+)(?::([0-9.]*)-?([0-9.]*))?")

  def loadModules: List[Module] = {
    (inputFiles.toList) map { inputFile =>
      try {
        val module = ModuleIO.readBinary(inputFile)
        val errors = module.validate
        if (!errors.isEmpty) {
          System.err.println("validation errors when reading file: " + inputFile)
          errors.foreach(System.err.println(_))
          System.exit(FAILURE_CODE)
        }
        module
      } catch {
        case exn: IOException => {
          exitWithFailure(inputFile + ": error: " + exn.getMessage); 
          null
        }
      }
    }
  }

  def linkModules(modules: List[Module]): Module = {
    val first = modules.head
    linkModules(modules,
                first.name,
                first.ty,
                first.version,
                first.filename,
                first.dependencies,
                first.searchPaths)
  }

  def linkModules(modules: List[Module],
                  name: Symbol,
                  ty: ModuleType,
                  version: Version,
                  filename: Option[File],
                  dependencies: List[ModuleDependency],
                  searchPaths: List[File]): Module = 
  {
    val is64Bit = checkWordSize(modules)
    val initialDepSet = (ListSet[ModuleDependency]() /: dependencies.reverse) { _ + _ }
    val allDependencies = (initialDepSet /: modules) { (deps, m) =>
      deps ++ m.dependencies
    }.toList.reverse
    val allDefinitions = for (module <- modules.iterator;
                              defn   <- module.definitions.valuesIterator)
                           yield (defn, module)
    val empty = new TreeMap[Symbol, (Definition, Module)]()
    val linkedDefinitions = (empty /: allDefinitions)(linkDefinition _).map { kv =>
      val (name, (definition, _)) = kv
      (name, definition)
    }
    val isSafe = (true /: modules.map(_.isSafe)) { _ & _ }
    new Module(name, ty, version, 
               filename, allDependencies, searchPaths, is64Bit, isSafe,
               linkedDefinitions)
  }

  def isStrong(defn: Definition): Boolean = {
    defn match {
      case f: Function if f.blocks.isEmpty => false
      case g: Global if !g.value.isDefined => false
      case _: Parameter => false
      case _ => true
    }
  }

  def linkDefinition(definitions: TreeMap[Symbol, (Definition, Module)], 
                     defn: (Definition, Module)): TreeMap[Symbol, (Definition, Module)] = 
  {
    val name = defn._1.name

    def conflictMessage(symbol: Symbol, message: String): String = {
      def filenameString(module: Module): String = {
        module.filename match {
          case Some(filename) => "(" + filename.toString + ")"
          case None => ""
        }
      }
      val oldModule = definitions(symbol)._2
      val newModule = defn._2
      "symbol %s %s:\n  %s %s\n  %s %s\n".
        format(symbol, message, 
               oldModule.name, filenameString(oldModule),
               newModule.name, filenameString(newModule))
    }

    def combine(oldDefn: (Definition, Module), 
                newDefn: (Definition, Module)) = {
      assert(oldDefn._1.name == newDefn._1.name)

      if (oldDefn._1 == newDefn._1)
        oldDefn
      else {
        if (oldDefn.getClass != newDefn.getClass)
          exitWithFailure(conflictMessage(name, "refers to different types of definitions"))

        if (isStrong(oldDefn._1) && isStrong(newDefn._1))
          exitWithFailure(conflictMessage(name, "refers to multiple strong definitions"))

        (oldDefn, newDefn) match {
          case ((oldFunction: Function, oldModule), (newFunction: Function, newModule)) => {
            if (oldFunction.ty(oldModule) != newFunction.ty(newModule))
              exitWithFailure(conflictMessage(name, "refers to functions with different types"))
          }
          case ((oldGlobal: Global, _), (newGlobal: Global, _)) => {
            if (oldGlobal.ty != newGlobal.ty)
              exitWithFailure(conflictMessage(name, "refers to globals with different types"))
          }
          case ((oldParameter: Parameter, _), (newParameter: Parameter, _)) => {
            if (oldParameter.ty != newParameter.ty)
              exitWithFailure(conflictMessage(name, "refers to parameters with different types"))
          }
        }

        if (isStrong(oldDefn._1))
          oldDefn
        else
          newDefn
      }
    }

    val combinedDefn = if (definitions.contains(name))
      combine(definitions(name), defn)
    else
      defn
    definitions + (name -> combinedDefn)
  }

  def checkWordSize(modules: List[Module]): Boolean = {
    val is64Bit = modules.headOption.map(_.is64Bit).getOrElse(true)
    val incorrectStr = if (is64Bit) "32-bit" else "64-bit"
    val correctStr = if (is64Bit) "64-bit" else "32-bit"
    def filenameStr(n: Option[File]) = {
      n match {
        case Some(f) => "from file \"" + f + "\" "
        case None => ""
      }
    }
    for (m <- modules) {
      if (is64Bit != m.is64Bit) {
        exitWithFailure("module %sis %s; expected %s".
                          format(filenameStr(m.filename), incorrectStr, correctStr))
      }
    }
    is64Bit
  }

  def validateOutput(module: Module, directory: File) {
    if (module.ty == ModuleType.LIBRARY || module.ty == ModuleType.PROGRAM) {
      val modules = try {
        Loader.loadDependenciesForModule(module, directory, List(module))
      } catch {
        case exn: IOException => {
          exitWithFailure(exn.getMessage)
          Nil
        }
      }

      val linkedModule = linkModules(modules,
                                     module.name,
                                     module.ty,
                                     module.version,
                                     module.filename,
                                     Nil,
                                     module.searchPaths)
      val errors = if (module.ty == ModuleType.PROGRAM)
        linkedModule.validateProgram
      else
        linkedModule.validateLibrary
      if (!errors.isEmpty)
        exitWithFailure("validation errors:\n" + errors.mkString("\n"))
    }
  }

  def defaultOutputFile: File = {
    import ModuleType._
    val nameStr = moduleName.toString
    val versionStr = if (moduleVersion == Version.MIN)
      ""
    else
      "-" + moduleVersion.toString
    val extensionStr = moduleTy match {
      case INTERMEDIATE => ".wo"
      case LIBRARY => ".wl"
      case PROGRAM => ".wp"
    }
    new File(nameStr + versionStr + extensionStr)
  }

  def storeModule(module: Module, file: File) {
    try {
      ModuleIO.writeBinary(module, file)
    } catch {
      case exn: IOException => exitWithFailure(exn.getMessage)
    }
  }
}
