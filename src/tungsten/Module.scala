package tungsten

import scala.collection.immutable.Map
import scala.collection.immutable.TreeMap
import scala.reflect.Manifest
import Utilities._

final class Module(val definitions: Map[Symbol, Definition]) {
  def this() = this(new TreeMap[Symbol, Definition])

  def add(defn: Definition): Module = {
    definitions.get(defn.name) match {
      case Some(d) => throw new RedefinedSymbolException(defn.name, defn.location, d.location)
      case None => new Module(definitions + (defn.name -> defn))
    }
  }

  def add(defn: Definition, defns: Definition*): Module = {
    val ds = defn :: defns.toList
    ds.foldLeft(this)(_.add(_))
  }

  def replace(defn: Definition) = new Module(definitions + (defn.name -> defn))

  def apply(name: Symbol): Definition = definitions(name)

  def get[T <: Definition](name: Symbol)(implicit m: Manifest[T]) = {
    definitions.get(name) match {
      case Some(d) if m.erasure.isInstance(d) => Some(d.asInstanceOf[T])
      case _ => None
    }
  }

  def getDefn(name: Symbol) = definitions.get(name)

  def getBlock(name: Symbol) = definitions(name).asInstanceOf[Block]
  def getBlocks(names: List[Symbol]) = names.map(getBlock _)
  def getField(name: Symbol) = definitions(name).asInstanceOf[Field]
  def getFields(names: List[Symbol]) = names.map(getField _)
  def getFunction(name: Symbol) = definitions(name).asInstanceOf[Function]
  def getFunctions(names: List[Symbol]) = names.map(getFunction _)
  def getGlobal(name: Symbol) = definitions(name).asInstanceOf[Global]
  def getGlobals(names: List[Symbol]) = names.map(getGlobal _)
  def getInstruction(name: Symbol) = definitions(name).asInstanceOf[Instruction]
  def getInstructions(names: List[Symbol]) = names.map(getInstruction _)
  def getParameter(name: Symbol) = definitions(name).asInstanceOf[Parameter]
  def getParameters(names: List[Symbol]) = names.map(getParameter _)
  def getStruct(name: Symbol) = definitions(name).asInstanceOf[Struct]
  def getStructs(names: List[Symbol]) = names.map(getStruct _)

  def validate = {
    def validateDependencies = {
      definitions.valuesIterable.flatMap(_.validateComponents(this)).toList
    }

    def validateDefinitions = {
      definitions.valuesIterable.flatMap(_.validate(this)).toList
    }

    def validateMain = {
      getDefn("main") match {
        case Some(main: Function) => {
          if (!main.parameters.isEmpty)
            List(MainNonEmptyParametersException(main.location))
          else if (main.returnType != UnitType())
            List(MainReturnTypeException(main.location))
          else
            Nil
        }
        case _ => List(MissingMainException())
      }
    }

    def validateStructDependencies = {
      def findStructDependencies(struct: Struct): Set[Symbol] = {
        val fieldTypes = struct.fields.map(getField(_).ty)
        val structDeps = fieldTypes.flatMap { fty =>
          fty match {
            case StructType(structName, _) => Some(structName)
            case _ => None
          }
        }
        structDeps.toSet
      }

      val structNames = definitions.valuesIterable.flatMap { defn =>
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
           val location = getStruct(scc.head).location)
        yield CyclicStructException(scc.toList, location)
    }

    stage(validateDependencies,
          validateDefinitions,
          validateMain ++ validateStructDependencies)
  }

  def validateName[T <: Definition](name: Symbol, 
                                    location: Location)
                                   (implicit m: Manifest[T]) =
  {
    getDefn(name) match {
      case Some(defn) if m.erasure.isInstance(defn) => Nil
      case Some(defn) => {
        List(InappropriateSymbolException(name,
                                          location,
                                          defn.location,
                                          humanReadableClassName[T]))
      }
      case None => List(UndefinedSymbolException(name, location))
    }
  }

  override def equals(that: Any) = {
    that.isInstanceOf[Module] && definitions == that.asInstanceOf[Module].definitions
  }

  override def hashCode = hash("Module", definitions)

  override def toString = definitions.valuesIterable.mkString("\n")
}
