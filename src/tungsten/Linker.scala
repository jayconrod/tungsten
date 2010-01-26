package tungsten

import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
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
  var moduleVersion: List[Int] = Nil
  val moduleDependencies = new ArrayBuffer[ModuleDependency]
  val moduleSearchPaths = new ArrayBuffer[File]

  var outputFile: Option[File] = None

  val inputFiles = new ArrayBuffer[File]

  def main(args: Array[String]) {
    parseArguments(args)

    val inputModules = loadModules
    val allDefinitions = for (module <- inputModules.iterator;
                              defn   <- module.definitions.valuesIterator)
                           yield (defn, module)
    val empty = Map[Symbol, (Definition, Module)]()
    val linkedDefinitions = allDefinitions.foldLeft(empty)(link _).map { kv =>
      val (name, (definition, module)) = kv
      (name, definition)
    }
    val outputModule = new Module(moduleName, 
                                  moduleTy, 
                                  moduleVersion, 
                                  moduleDependencies.toList, 
                                  moduleSearchPaths.toList, 
                                  linkedDefinitions)
    storeModule(outputModule)
  }

  def parseArguments(args: Array[String]) {
    var i = 0
    var errorMessage: Option[String] = None
    def next: String = {
      if (i + 1 < args.size) {
        i += 1
        args(i)
      } else {
        usage
        System.exit(ERROR_CODE)
        ""
      }
    }

    while (i < args.size && !errorMessage.isDefined) {
      args(i) match {
        case "-n" => {
          val nameStr = next
          try {
            moduleName = symbolFromString(nameStr)
          } catch {
            case _: IllegalArgumentException => errorMessage = Some("Invalid module name")
          }
        }
        case "-t" => {
          import ModuleType._
          next match {
            case "program" => moduleTy = PROGRAM
            case "library" => moduleTy = LIBRARY
            case "intermediate" => moduleTy = INTERMEDIATE
            case _ => errorMessage = Some("Invalid type. Must be program, library, or intermediate.")
          }
        }
        case "-v" => {
          try {
            moduleVersion = parseVersion(next)
          } catch {
            case _ => errorMessage = Some("Invalid version. Must be dot-separated integers.")
          }
        }
        case "-o" => outputFile = Some(new File(next))
        case "-L" => {
          moduleSearchPaths ++= next.split(File.pathSeparator).map(new File(_))
        }
        case LibraryArgument(nameStr, minVersionStr, maxVersionStr) => {
          try {
            val name = symbolFromString(nameStr)
            val minVersion = parseVersion(minVersionStr)
            val maxVersion = parseVersion(maxVersionStr)
            moduleDependencies += new ModuleDependency(name, minVersion, maxVersion)
          } catch {
            case _ => errorMessage = Some("Invalid library: " + args(i))
          }
        }
        case _ if args(i).startsWith("_") => {
          errorMessage = Some("Invalid option: " + args(i))
        }
        case _ => inputFiles += new File(args(i))
      }
      i += 1
    }

    if (errorMessage.isDefined)
      exitWithError(errorMessage.get)
  }

  def parseVersion(versionStr: String): List[Int] = {
    if (versionStr == null || versionStr.isEmpty)
      Nil
    else {
      val version = versionStr.split("\\.").map(_.toInt)
      if (version.exists(_ < 0))
        throw new IllegalArgumentException
      else
        version.toList
    }
  }

  val LibraryArgument = new Regex("-l([A-Za-z0-9_$.]+)(?::([0-9.]*)-?([0-9.]*))?")

  def loadModules: List[Module] = {
    (inputFiles.toList) map { inputFile =>
      try {
        val module = ModuleIO.readBinary(inputFile)
        val errors = module.validate
        if (!errors.isEmpty) {
          errors.foreach(System.err.println(_))
          System.exit(FAILURE_CODE)
        }
        module
      } catch {
        case exn: IOException => {
          exitWithFailure(exn.getMessage); 
          null
        }
      }
    }
  }

  def isStrong(defn: Definition): Boolean = {
    defn match {
      case Function(_, _, _, Nil, _) => false
      case Global(_, _, None, _) => false
      case _: Parameter => false
      case _ => true
    }
  }

  def link(definitions: Map[Symbol, (Definition, Module)], 
           defn: (Definition, Module)): Map[Symbol, (Definition, Module)] = 
  {
    val name = defn._1.name

    def combine(oldDefn: (Definition, Module), 
                newDefn: (Definition, Module)) = {
      assert(oldDefn._1.name == newDefn._1.name)

      if (oldDefn.getClass != newDefn.getClass)
        exitWithFailure("symbol %s refers to different types of definitions".format(name))

      if (isStrong(oldDefn._1) && isStrong(newDefn._1))
        exitWithFailure("symbol %s refers to multiple strong definitions".format(name))

      (oldDefn, newDefn) match {
        case ((oldFunction: Function, oldModule), (newFunction: Function, newModule)) => {
          if (oldFunction.ty(oldModule) != newFunction.ty(newModule))
            exitWithFailure("symbol %s refers to functions with different types".format(name))
        }
        case ((oldGlobal: Global, _), (newGlobal: Global, _)) => {
          if (oldGlobal.ty != newGlobal.ty)
            exitWithFailure("symbol %s refers to globals with different types".format(name))
        }
        case ((oldParameter: Parameter, _), (newParameter: Parameter, _)) => {
          if (oldParameter.ty != newParameter.ty)
            exitWithFailure("symbol %s refers to parameters with different types".format(name))
        }
      }

      if (isStrong(oldDefn._1))
        oldDefn
      else
        newDefn
    }

    val combinedDefn = if (definitions.contains(name))
      combine(definitions(name), defn)
    else
      defn
    definitions + (name -> combinedDefn)
  }

  def storeModule(module: Module) = {
    val outFile = outputFile match {
      case Some(f) => f
      case None => {
        import ModuleType._
        val nameStr = moduleName.toString
        val versionStr = if (moduleVersion.isEmpty)
          ""
        else
          "-" + moduleVersion.mkString(".")
        val extensionStr = moduleTy match {
          case INTERMEDIATE => ".wo"
          case LIBRARY => ".wl"
          case PROGRAM => ".wp"
        }
        new File(nameStr + versionStr + extensionStr)
      }
    }

    try {
      ModuleIO.writeBinary(module, outFile)
    } catch {
      case exn: IOException => exitWithFailure(exn.getMessage)
    }
  }
}
