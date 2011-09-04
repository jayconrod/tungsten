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
                   val definitions:  Map[Symbol, Definition]     = new TreeMap())
{
  def copyWith(name: Symbol = name,
               ty: ModuleType = ty,
               version: Version = version,
               filename: Option[File] = filename,
               dependencies: List[ModuleDependency] = dependencies,
               searchPaths: List[File] = searchPaths,
               is64Bit: Boolean = is64Bit,
               definitions: Map[Symbol, Definition] = definitions) =
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

  def get[T <: Definition](name: Symbol)(implicit m: Manifest[T]): Option[T] = {
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
  def getObjectDefinition(name: Symbol) = definitions(name).asInstanceOf[ObjectDefinition]
  def getObjectDefinitions(names: List[Symbol]) = names.map(getObjectDefinition _)
  def getParameter(name: Symbol) = definitions(name).asInstanceOf[Parameter]
  def getParameters(names: List[Symbol]) = names.map(getParameter _)
  def getStruct(name: Symbol) = definitions(name).asInstanceOf[Struct]
  def getStructs(names: List[Symbol]) = names.map(getStruct _)
  def getTypeParameter(name: Symbol) = definitions(name).asInstanceOf[TypeParameter]
  def getTypeParameters(names: List[Symbol]) = names.map(getTypeParameter _)

  def highestSymbolId: Int = {
    definitions.keys.map(_.id).max
  }

  lazy val rootClass: Class = {
    val rootClasses = definitions.values.collect {
      case defn: Class if !defn.superclass.isDefined => defn
    }
    if (rootClasses.isEmpty)
      throw new RuntimeException("no root class is defined!")
    else if (rootClasses.size > 1)
      throw new RuntimeException("multiple root classes are defined")
    else if (rootClasses.head.typeParameters.size > 0)
      throw new RuntimeException("root class must not be parameterized")
    else
      rootClasses.head
  }

  lazy val rootClassType: ClassType = ClassType(rootClass.name, Nil)

  /** Performs a thorough check of invariants required by Tungsten and returns any
   *  errors found. See design/validation.txt for a list of checks performed. Note that
   *  some errors may prevent further checking, so the returned list of errors is not
   *  guaranteed to be complete.
   */
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
      definitions.values.toList.flatMap { defn =>
        val location = defn.getLocation
        def validateTypeComponents(errors: List[CompileException], ty: Type) = {
          ty.validateComponents(this, location) ++ errors
        }
        def validateValueComponents(errors: List[CompileException], value: Value) = {
          value.validateComponents(this, location) ++ errors
        }

        defn.validateComponents(this) ++
          defn.foldTypes[List[CompileException]](Nil, validateTypeComponents _) ++
          defn.foldValues[List[CompileException]](Nil, validateValueComponents _)
      }
    }

    def validateScope = {
      val globalDefns = definitions.values.filter(_.isGlobal)
      val globalScope = globalDefns.map(_.name).toSet
      globalDefns.toList.flatMap(_.validateScope(this, globalScope))
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

    def validateTypeParameters = {
      val typeParameters = definitions.values.collect { case t: TypeParameter => t }
      typeParameters.flatMap(_.validate(this)).toList
    }

    def validateDefinitions = {
      val otherDefns = definitions.values.collect { case d if !d.isInstanceOf[TypeParameter] => d }
      otherDefns.flatMap(_.validate(this)).toList
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

    def validateRootClass = {
      val rootClasses = definitions.values.collect {
        case clas: Class if !clas.superclass.isDefined => clas
      }
      if (rootClasses.size > 1) {
        rootClasses.toList.map { clas => 
          MultipleRootClassException(clas.name, clas.getLocation) 
        }
      } else {
        rootClasses.headOption match {
          case Some(clas) if !clas.typeParameters.isEmpty =>
            List(ParameterizedRootClassException(clas.name, clas.getLocation))
          case _ => Nil
        }
      }
    }

    def validateCycles[T <: Definition](findDependencies: T => Set[Symbol],
                                        generateError: (List[Symbol], Location) => CompileException)
                                       (implicit m: Manifest[T]): List[CompileException] =
    {
      val matchedDefns = definitions.values.collect {
        case defn if m.erasure.isInstance(defn) => defn.asInstanceOf[T]
      }
      val dependencyMap = (Map[Symbol, Set[Symbol]]() /: matchedDefns) { (deps, defn) =>
        deps + (defn.name -> findDependencies(defn))
      }
      val dependencyGraph = new Graph(matchedDefns.map(_.name), dependencyMap)
      val sccDependencyGraph = dependencyGraph.findSCCs
      for (scc <- sccDependencyGraph.nodes.toList;
           if sccDependencyGraph.adjacent(scc).contains(scc);
           val location = definitions(scc.head).getLocation)
        yield generateError(scc.toList, location)
    }

    def validateStructCycles = {
      def findDependencies(struct: Struct): Set[Symbol] = {
        val fieldTypes = struct.fields.map(getField(_).ty)
        val structDeps = fieldTypes.collect { case StructType(structName) => structName }
        structDeps.toSet
      }
      val generateError = CyclicStructException.apply _
      validateCycles(findDependencies _, generateError)
    }

    def validateInheritanceCycles = {
      def findDependencies(defn: ObjectDefinition): Set[Symbol] = {
        val inheritedTypes = defn.getSuperType.toList ++ defn.interfaceTypes
        inheritedTypes.map(_.definitionName).toSet
      }
      val generateError = CyclicInheritanceException.apply _
      validateCycles(findDependencies, generateError)
    }

    def validateTypeParameterCycles = {
      def findDependencies(tyParam: TypeParameter): Set[Symbol] = {
        val types = tyParam.upperBound.toList ++ tyParam.lowerBound.toList
        types.collect { case VariableType(tyParamName, _) => tyParamName }.toSet
      }
      val generateError = CyclicTypeParameterException.apply _
      validateCycles(findDependencies, generateError)
    }

    /* Illegal inheritance occurs when the superclass of an object definition is not a
     * subclass of all inherited interface superclasses.
     */
    def validateIllegalInheritance = {
      def findInterfaceSuperclass(interface: Interface,
                                  superclasses: Map[Symbol, Class]): Map[Symbol, Class] =
      {
        if (superclasses.contains(interface.name))
          superclasses
        else {
          interface.supertype match {
            case ClassType(parentName, _, _) => {
              val parent = getClass(parentName)
              superclasses + (interface.name -> parent)
            }
            case InterfaceType(parentName, _, _) => {
              val parent = getInterface(parentName)
              val superclassesWithParent = findInterfaceSuperclass(parent, superclasses)
              superclassesWithParent + (interface.name -> superclassesWithParent(parentName))
            }
          }
        }
      }
      val interfaces = definitions.values.collect { case i: Interface => i }.toList
      val interfaceSuperclasses = (Map[Symbol, Class]() /: interfaces) { (sc, i) =>
        findInterfaceSuperclass(i, sc)
      }

      def findClassSuperclass(clas: Class,
                              superclasses: Map[Symbol, Class]): Map[Symbol, Class] =
      {
        val superclass = clas.superclass match {
          case Some(classTy) => getClass(classTy.className)
          case None => clas
        }
        superclasses + (clas.name -> clas)
      }        
      val classes = definitions.values.collect { case c: Class => c }.toList
      val classSuperclasses = (Map[Symbol, Class]() /: classes) { (sc, c) =>
        findClassSuperclass(c, sc)
      }
      val superclasses = interfaceSuperclasses ++ classSuperclasses

      for (defn <- classes ++ interfaces;
           val superclass = superclasses(defn.name);
           interfaceType <- defn.interfaceTypes;
           val interfaceSuperclass = superclasses(interfaceType.interfaceName);
           if !superclass.isSubclassOf(interfaceSuperclass, this))
        yield IllegalInheritanceException(defn.name, defn.getLocation)
    }

    /* Conflicting inheritance occurs when a class or interface inherits another class
     * or interface in more than one way (by more than one path in the inheritance graph),
     * and there are multiple distinct types for the same inherited definition.
     */
    def validateConflictingInheritance = {
      // Construct the inheritance graph
      val defns = definitions.values.collect { case d: ObjectDefinition => d }
      val adjacent = (Map[Symbol, Set[Symbol]]() /: defns) { case (adj, defn) =>
        adj + (defn.name -> defn.inheritedTypes.toSet.map { t: ObjectDefinitionType => 
          t.definitionName
        })
      }
      val inheritanceGraph = new Graph(defns.map(_.name), adjacent)

      // This function will process each definition recursively in depth first order. 
      // Each definition will only be processed once. A set of visited definition names is
      // returned, as long as the types inherited by each pair of definitions. There should
      // only be one type in each of these inheritance sets.
      def visit(defn: ObjectDefinition,
                visited: Set[Symbol],
                inheritedTypes: Map[(Symbol, Symbol), Set[ObjectType]],
                typesFromParent: Set[ObjectDefinitionType],
                parent: Option[ObjectDefinition]): (Set[Symbol], Map[(Symbol, Symbol), Set[ObjectType]]) =
      {
        val substitutedTypesFromParent: Set[ObjectDefinitionType] = parent match {
          case Some(parentDefn) => {
            val typeArguments = defn.getInheritedType(parentDefn.name).typeArguments
            typesFromParent.map { ty => 
              parentDefn.substituteInheritedType(ty, typeArguments) 
            }
          }
          case None => Set()
        }
        val typesFromSelf = substitutedTypesFromParent + defn.selfType

        val newInheritedTypes = (inheritedTypes /: typesFromSelf) { (inheritedTypes, ty) =>
          val key = (defn.name, ty.definitionName)
          val types = inheritedTypes.getOrElse(key, Set[ObjectType]())
          inheritedTypes + (key -> (types + ty))
        }
        if (visited(defn.name))
          (visited, newInheritedTypes)
        else {
          val newVisited = visited + defn.name
          ((newVisited, newInheritedTypes) /: inheritanceGraph.incident(defn.name)) { (ind, childName) =>
            val (visited, inheritedTypes) = ind
            visit(get[ObjectDefinition](childName).get,
                  visited,
                  inheritedTypes,
                  typesFromSelf,
                  Some(defn))
          }
        }
      }

      // Process the inheritance graph and flag each pair of definitions with more than
      // one inherited type.
      val roots = inheritanceGraph.nodes.filter(inheritanceGraph.adjacent(_).isEmpty)
      val emptyVisited = Set[Symbol]()
      val emptyInheritedTypes = Map[(Symbol, Symbol), Set[ObjectType]]()
      val (_, inheritedTypes) = ((emptyVisited, emptyInheritedTypes) /: roots) { (ind, rootName) =>
        val defn = getObjectDefinition(rootName)
        val (visited, inheritedTypes) = ind
        visit(defn, visited, inheritedTypes, Set(), None)
      }

      inheritedTypes.collect { case ((defnName, ancestorName), types) if types.size > 1 =>
        InheritanceConflictException(defnName, ancestorName, definitions(defnName).getLocation)
      }.toList
    }

    validateDependencies ++?
      validateComponents ++?
      validateScope ++?
      validateRootClass ++?
      (validateStructCycles ++ 
         validateInheritanceCycles ++ 
         validateTypeParameterCycles) ++?
       validateIllegalInheritance ++?
       validateConflictingInheritance ++?
       validateTypes ++?
       validateValues ++?
       validateTypeParameters ++?
       validateDefinitions ++?
       validateMain
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
