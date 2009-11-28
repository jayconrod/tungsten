package tungsten

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.reflect.Manifest
import Utilities._

final class Module {
  private val _definitions: Map[Symbol, Definition] = new HashMap[Symbol, Definition]

  def definitions = _definitions

  def add(defn: Definition) = {
    _definitions.get(defn.name) match {
      case Some(d) => throw new RedefinedSymbolException(defn.name, defn.location, d.location)
      case None => _definitions += defn.name -> defn
    }
    ()
  }

  def update(defn: Definition) = {
    _definitions += defn.name -> defn
  }

  def getDefn(name: Symbol): Option[Definition] = _definitions.get(name)

  def get[T <: Definition](name: Symbol)(implicit m: Manifest[T]): Option[T] = {
    _definitions.get(name) match {
      case Some(d) if m.erasure.isInstance(d) => Some(d.asInstanceOf[T])
      case _ => None
    }
  }

  def getBlock(name: Symbol) = get[Block](name).get
  def getBlocks(names: List[Symbol]) = names.map(getBlock _)
  def getClass(name: Symbol) = get[Class](name).get
  def getClasses(names: List[Symbol]) = names.map(getClass _)
  def getField(name: Symbol) = get[Field](name).get
  def getFields(names: List[Symbol]) = names.map(getField _)
  def getFunction(name: Symbol) = get[Function](name).get
  def getFunctions(names: List[Symbol]) = names.map(getFunction _)
  def getGlobal(name: Symbol) = get[Global](name).get
  def getGlobals(names: List[Symbol]) = names.map(getGlobal _)
  def getInstruction(name: Symbol) = get[Instruction](name).get
  def getInstructions(names: List[Symbol]) = names.map(getInstruction _)
  def getInterface(name: Symbol) = get[Interface](name).get
  def getInterfaces(names: List[Symbol]) = names.map(getInterface _)
  def getParameter(name: Symbol) = get[Parameter](name).get
  def getParameters(names: List[Symbol]) = names.map(getParameter _)
  def getStruct(name: Symbol) = get[Struct](name).get
  def getStructs(names: List[Symbol]) = names.map(getStruct _)
  def getTypeParameter(name: Symbol) = get[TypeParameter](name).get
  def getTypeParameters(names: List[Symbol]) = names.map(getTypeParameter _)

  def validate = {
    def validateDependencies = {
      val allErrors = for (defn <- _definitions.valueIterable)
        yield defn.validateComponents(this)
      allErrors.flatten.toList
    }

    def validateDefinitions = {
      val allErrors = for (defn <- _definitions.valueIterable)
        yield defn.validate(this)
      allErrors.flatten.toList
    }

    def validateMain = {
      _definitions.get(new Symbol("main")) match {
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

    stage(validateDependencies,
          validateDefinitions,
          validateMain)
  }

  override def equals(that: Any) = that match {
    case m: Module => {
      _definitions equals m._definitions
    }
    case _ => false
  }

  override def hashCode = _definitions.hashCode

  override def toString = _definitions.toString
}
