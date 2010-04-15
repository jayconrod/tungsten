package tungsten.llvm

import scala.collection.mutable.{Set => MSet}
import tungsten.Symbol

object LlvmToTungstenConverter {
  def convert(module: Module): tungsten.Module = {
    val cDefinitions = MSet[tungsten.Definition]()
    module.definitions.foreach { (defn) =>
      val definition = defn._2
      definition match {
        case function: Function => convertFunction(function, cDefinitions)
        case _ => throw new UnsupportedOperationException
      }
    }
    val defnMap = (Map[Symbol, tungsten.Definition]() /: cDefinitions) { (defnMap, definition) =>
      defnMap + (definition.name -> definition)
    }
    new tungsten.Module(defnMap)
  }

  def convertFunction(function: Function, 
                      cDefinitions: MSet[tungsten.Definition]): tungsten.Function =
  {
    val cName = new Symbol(function.name)
    val cReturnType = convertType(function.returnType)
    val cParameters = function.parameters.map(convertParameter(_, cName, cDefinitions))
    val cBlocks = function.blocks.map(convertBlock(_, cName, cDefinitions))
    val cFunction = tungsten.Function(cName, 
                                      cParameters.map(_.name),
                                      cReturnType,
                                      cBlocks.map(_.name))
    cDefinitions += cFunction
    cFunction
  }

  def convertParameter(parameter: Parameter,
                       parentName: Symbol,
                       cDefinitions: MSet[tungsten.Definition]): tungsten.Parameter =
  {
    val cName = parentName + parameter.name
    val cType = convertType(parameter.ty)
    val cParam = tungsten.Parameter(cName, cType)
    cDefinitions += cParam
    cParam
  }

  def convertBlock(block: Block,
                   parentName: Symbol,
                   cDefinitions: MSet[tungsten.Definition]): tungsten.Block =
  {
    throw new UnsupportedOperationException
  }

  def convertType(ty: Type): tungsten.Type = {
    ty match {
      case VoidType => tungsten.UnitType()
      case IntType(n) => {
        if (n == 1)
          tungsten.BooleanType()
        else if (n <= 8)
          tungsten.IntType(8)
        else if (n <= 16)
          tungsten.IntType(16)
        else if (n <= 32)
          tungsten.IntType(32)
        else if (n <= 64)
          tungsten.IntType(64)
        else
          throw new UnsupportedOperationException
      }
      case PointerType(ety) => tungsten.PointerType(convertType(ety))
      case LabelType => throw new UnsupportedOperationException
    }
  }

  def convertValue(value: Value, parent: Option[Symbol] = None): tungsten.Value = {
    value match {
      case IntValue(n, width) => {
        if (width == 1)
          tungsten.BooleanValue(if (n == 0L) false else true)
        else if (width <= 8)
          tungsten.Int8Value(n.toByte)
        else if (width <= 16)
          tungsten.Int16Value(n.toShort)
        else if (width <= 32)
          tungsten.Int32Value(n.toInt)
        else if (width <= 64)
          tungsten.Int64Value(n)
        else
          throw new UnsupportedOperationException
      }
      case DefinedValue(name, _) => {
        val sym = parent match {
          case Some(p) => p + name
          case None => Symbol(name)
        }
        tungsten.DefinedValue(sym)
      }
    }
  }
}

