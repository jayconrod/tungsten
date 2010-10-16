package tungsten

import scala.collection.immutable.Map
import scala.collection.immutable.TreeMap
import scala.reflect.Manifest
import java.io.File
import Utilities._

final class Module(val name:         Symbol                      = Symbol("default"),
                   val ty:           ModuleType                  = ModuleType.INTERMEDIATE,
                   val version:      Version                     = Version.MIN,
                   val filename:     Option[File]                = None,
                   val dependencies: List[ModuleDependency]      = Nil,
                   val searchPaths:  List[File]                  = Nil,
                   val is64Bit:      Boolean                     = true,
                   val isSafe:       Boolean                     = false,
                   val definitions:  TreeMap[Symbol, Definition] = new TreeMap())
{
  def copyWith(name: Symbol = name,
               ty: ModuleType = ty,
               version: Version = version,
               filename: Option[File] = filename,
               dependencies: List[ModuleDependency] = dependencies,
               searchPaths: List[File] = searchPaths,
               is64Bit: Boolean = is64Bit,
               definitions: TreeMap[Symbol, Definition] = definitions) =
  {
    new Module(name, ty, version, filename, dependencies, searchPaths, is64Bit, isSafe, definitions)
  }

  def add(defns: Definition*): Module = {
    val newDefinitions = (definitions /: defns) { (map, d) =>
      map.get(d.name) match {
        case Some(orig) => throw new RedefinedSymbolException(d.name, d.getLocation, orig.getLocation)
        case None => map + (d.name -> d)
      }
    }
    copyWith(definitions = newDefinitions)
  }

  def replace(defn: Definition) = copyWith(definitions = definitions + (defn.name -> defn))

  def replace(replacements: Definition*): Module = {
    val newDefinitions = (definitions /: replacements) { (definitions, replacement) =>
      definitions + (replacement.name -> replacement)
    }
    copyWith(definitions=newDefinitions)
  }

  def remove(symbol: Symbol) = copyWith(definitions = definitions - symbol)

  def remove(symbols: Seq[Symbol]) = copyWith(definitions = definitions -- symbols)

  def apply(name: Symbol): Definition = definitions(name)

  def get[T <: Definition](name: Symbol)(implicit m: Manifest[T]) = {
    definitions.get(name) match {
      case Some(d) if m.erasure.isInstance(d) => Some(d.asInstanceOf[T])
      case _ => None
    }
  }

  def mapSymbols(function: Symbol => Symbol): Module = {
    val empty = new TreeMap[Symbol, Definition]()
    val newDefinitions = (empty /: definitions.values) { (defns, defn) =>
      val newDefn = defn.mapSymbols(function)
      if (defns.contains(newDefn.name))
        throw new RuntimeException("mapSymbols gave multiple definitions the same name")
      else
        defns + (newDefn.name -> newDefn)
    }
    copyWith(definitions=newDefinitions)
  }

  def mapValues(function: Value => Value): Module = {
    val empty = new TreeMap[Symbol, Definition]()
    val newDefinitions = (empty /: definitions.values) { (defns, defn) =>
      val newDefn = defn.mapValues(function)
      defns + (newDefn.name -> newDefn)
    }
    copyWith(definitions=newDefinitions)
  }

  def mapTypes(function: Type => Type): Module = {
    val empty = new TreeMap[Symbol, Definition]()
    val newDefinitions = (empty /: definitions.values) { (defns, defn) =>
      val newDefn = defn.mapTypes(function)
      defns + (newDefn.name -> newDefn)
    }
    copyWith(definitions=newDefinitions)
  }

  def foldSymbols[A](accum: A, function: (A, Symbol) => A) = {
    (accum /: definitions.values) { (a, d) => d.foldSymbols(a, function) }
  }

  def foldValues[A](accum: A, function: (A, Value) => A) = {
    (accum /: definitions.values) { (a, d) => d.foldValues(a, function) }
  }

  def foldTypes[A](accum: A, function: (A, Type) => A) = {
    (accum /: definitions.values) { (a, d) => d.foldTypes(a, function) }
  }

  def getDefn(name: Symbol) = definitions.get(name)

  def getAnnotation(name: Symbol) = definitions(name).asInstanceOf[Annotation]
  def getAnnotations(names: List[Symbol]) = names.map(getAnnotation _)
  def getBlock(name: Symbol) = definitions(name).asInstanceOf[Block]
  def getBlocks(names: List[Symbol]) = names.map(getBlock _)
  def getClass(name: Symbol) = definitions(name).asInstanceOf[Class]
  def getClasses(names: List[Symbol]) = names.map(getClass _)
  def getField(name: Symbol) = definitions(name).asInstanceOf[Field]
  def getFields(names: List[Symbol]) = names.map(getField _)
  def getFunction(name: Symbol) = definitions(name).asInstanceOf[Function]
  def getFunctions(names: List[Symbol]) = names.map(getFunction _)
  def getGlobal(name: Symbol) = definitions(name).asInstanceOf[Global]
  def getGlobals(names: List[Symbol]) = names.map(getGlobal _)
  def getInterface(name: Symbol) = definitions(name).asInstanceOf[Interface]
  def getInterfaces(names: List[Symbol]) = names.map(getInterface _)
  def getInstruction(name: Symbol) = definitions(name).asInstanceOf[Instruction]
  def getInstructions(names: List[Symbol]) = names.map(getInstruction _)
  def getParameter(name: Symbol) = definitions(name).asInstanceOf[Parameter]
  def getParameters(names: List[Symbol]) = names.map(getParameter _)
  def getStruct(name: Symbol) = definitions(name).asInstanceOf[Struct]
  def getStructs(names: List[Symbol]) = names.map(getStruct _)
  def getTypeParameter(name: Symbol) = definitions(name).asInstanceOf[TypeParameter]
  def getTypeParameters(names: List[Symbol]) = names.map(getTypeParameter _)

  def validate: List[CompileException] = {
    def validateDependencies = {
      def checkDuplicates(depNames: Set[Symbol],
                          dependencies: List[ModuleDependency],
                          duplicates: Set[Symbol]): Set[Symbol] =
      {
        dependencies match {
          case Nil => duplicates
          case h :: t => {
            val newDuplicates = if (depNames(h.name))
              duplicates + h.name
            else
              duplicates
            checkDuplicates(depNames + h.name, t, newDuplicates)
          }
        }
      }
      val duplicates = checkDuplicates(Set[Symbol](), dependencies, Set[Symbol]())
      duplicates.toList.map(DuplicateDependencyException(name, _))
    }
  
    def validateComponents = {
      definitions.values.flatMap(_.validateComponents(this)).toList
    }

    def validateTypes = {
      definitions.values.toList.flatMap { defn =>
        defn.foldTypes[List[CompileException]](Nil, { (errors, ty) =>
          ty.validate(this, defn.getLocation) ++ errors
        })
      }
    }

    def validateValues = {
      definitions.values.toList.flatMap { defn =>
        defn.foldValues[List[CompileException]](Nil, { (errors: List[CompileException], value) =>
          value.validate(this, defn.getLocation) ++ errors
        })
      }
    }

    def validateDefinitions = {
      definitions.values.flatMap(_.validate(this)).toList
    }

    def validateMain = {
      getDefn("main") match {
        case Some(main: Function) => {
          if (!main.parameters.isEmpty)
            List(MainNonEmptyParametersException(main.getLocation))
          else if (main.returnType != UnitType)
            List(MainReturnTypeException(main.getLocation))
          else
            Nil
        }
        case _ => Nil
      }
    }

    def validateStructDependencies = {
      def findStructDependencies(struct: Struct): Set[Symbol] = {
        val fieldTypes = struct.fields.map(getField(_).ty)
        val structDeps = fieldTypes.flatMap { fty =>
          fty match {
            case StructType(structName) => Some(structName)
            case _ => None
          }
        }
        structDeps.toSet
      }

      val structNames = definitions.values.flatMap { defn =>
        defn match {
          case s: Struct => Some(s.name)
          case _ => None
        }
      }      
      val dependencyMap = structNames.foldLeft(Map[Symbol, Set[Symbol]]()) { (deps, structName) =>
        deps + (structName -> findStructDependencies(getStruct(structName)))
      }
      val dependencyGraph = new Graph[Symbol](structNames, dependencyMap)
      val sccDependencyGraph = dependencyGraph.findSCCs
      for (scc <- sccDependencyGraph.nodes;
           if sccDependencyGraph.adjacent(scc).contains(scc);
           val location = getStruct(scc.head).getLocation)
        yield CyclicStructException(scc.toList, location)
    }

    stage(validateDependencies,
          validateComponents,
          validateTypes,
          validateValues,
          validateDefinitions,
          validateMain ++ validateStructDependencies)
  }

  def validateProgram: List[CompileException] = {
    def validateHasMain = {
      if (!definitions.contains("main"))
        List(MissingMainException())
      else
        Nil
    }

    validateHasMain ++ validateIsLinked
  }

  def validateLibrary = validateIsLinked

  def validateIsLinked: List[CompileException] = {
    def isDefined(defn: Definition) = {
      defn match {
        case f: Function if f.blocks == Nil => false
        case _ => true
      }
    }

    definitions.
      valuesIterator.
      filter(!isDefined(_)).
      map { (defn: Definition) => ExternalDefinitionException(defn.name) }.
      toList
  }

  def validateName[T <: Definition](name: Symbol, 
                                    location: Location)
                                   (implicit m: Manifest[T]) =
  {
    getDefn(name) match {
      case Some(defn) if m.erasure.isInstance(defn) => Nil
      case Some(defn) => {
        List(InappropriateSymbolException(name, location, humanReadableClassName[T]))
      }
      case None => List(UndefinedSymbolException(name, location))
    }
  }

  override def equals(that: Any) = {
    that match {
      case m: Module => {
        name         == m.name         &&
        ty           == m.ty           &&
        version      == m.version      &&
        dependencies == m.dependencies &&
        searchPaths  == m.searchPaths  &&
        is64Bit      == m.is64Bit      &&
        isSafe       == m.isSafe       &&
        definitions  == m.definitions
      }
      case _ => false
    }
  }

  override def hashCode = {
    hash("Module", name, ty, version, dependencies, searchPaths, is64Bit, isSafe, definitions)
  }

  override def toString = {
    "name: " + name + "\n" +
    "type: " + ty + "\n" +
    "version: " + version + "\n" +
    "filename: " + filename + "\n" +
    "dependencies: " + dependencies.mkString(", ") + "\n" +
    "searchpaths: " + searchPaths.mkString(", ") + "\n" +
    "is64bit: " + is64Bit + "\n" +
    "safe: " + isSafe + "\n\n" +
    definitions.values.mkString("\n")
  }
}

final class ModuleType(description: String) {
  override def toString = description
}
object ModuleType {
  val INTERMEDIATE = new ModuleType("intermediate")
  val LIBRARY = new ModuleType("library")
  val PROGRAM = new ModuleType("program")
}

final case class ModuleDependency(name: Symbol,
                                  minVersion: Version,
                                  maxVersion: Version)
{
  override def toString = {
    val minVersionStr = if (minVersion == Version.MIN) "" else minVersion.toString
    val maxVersionStr = if (maxVersion == Version.MAX) "" else maxVersion.toString
    val versionStr = if (minVersionStr.isEmpty && maxVersionStr.isEmpty)
      ""
    else
      ":" + minVersionStr + "-" + maxVersionStr
    name.toString + versionStr
  }
}
