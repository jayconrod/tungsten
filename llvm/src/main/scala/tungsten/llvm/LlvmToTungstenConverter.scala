package tungsten.llvm

import tungsten.Symbol

class LlvmToTungstenConverter(val module: Module) {
  var cDefinitions = Map[Symbol, tungsten.Definition]()
  val symbolFactory = new tungsten.SymbolFactory
  var parents: List[String] = Nil

  def convert: tungsten.Module = {
    module.definitions.foreach { (defn) =>
      val definition = defn._2
      definition match {
        case function: Function => convertFunction(function)
        case _ => throw new UnsupportedOperationException
      }
    }
    new tungsten.Module(cDefinitions)
  }

  def convertFunction(function: Function): tungsten.Function =
  {
    parents ::= function.name
    val cName = new Symbol(function.name)
    val cReturnType = convertType(function.returnType)
    val cParameters = function.parameters.map(convertParameter _)
    val cBlocks = function.blocks.map(convertBlock _)
    val cFunction = tungsten.Function(cName, 
                                      cParameters.map(_.name),
                                      cReturnType,
                                      cBlocks.map(_.name))
    cDefinitions += cFunction.name -> cFunction
    parents = parents.tail
    cFunction
  }

  def convertParameter(parameter: Parameter): tungsten.Parameter = {
    val cName = makeChildSymbol(parameter.name)
    val cType = convertType(parameter.ty)
    val cParam = tungsten.Parameter(cName, cType)
    cDefinitions += cParam.name -> cParam
    cParam
  }

  def convertBlock(block: Block): tungsten.Block = {
    throw new UnsupportedOperationException
  }

  def convertInstruction(instruction: Instruction): tungsten.Instruction = {
    val name = instruction.name
    val cName = if (name.isEmpty)
      makeAnonymousChildSymbol
    else
      makeChildSymbol(name)

    val cInst = instruction match {
      case AllocaInstruction(_, elementType) => {
        val ty = tungsten.PointerType(convertType(elementType))
        tungsten.StackAllocateInstruction(cName, ty)
      }

      case BitcastInstruction(_, value, ty) => {
        (value.ty, ty) match {
          case (IntType(n), IntType(m)) if n == m => {
            tungsten.AssignInstruction(cName, convertValue(value))
          }
          case (PointerType(fromEty), PointerType(toEty)) => {
            // TODO
            throw new UnsupportedOperationException
          }
          case _ => throw new LlvmConversionException("could not bitcast from type " + value.ty + " to type " + ty)
        }
      }

      case BranchInstruction(label) => {
        label match {
          case DefinedValue(labelName, LabelType) => {
            assert(parents.size == 2)
            val functionName = parents.last
            val blockSymbol = Symbol(List(functionName, labelName))
            module.definitions.get(functionName) match {
              case Some(Function(_, _, _, _, blocks)) if blocks.exists(_.name == labelName) => {
                // TODO: handle argument passing over blocks
                tungsten.BranchInstruction(cName, blockSymbol, Nil)
              }
              case _ => throw new LlvmConversionException("could not branch to invalid label: " + labelName)
            }
          }
          case _ => throw new LlvmConversionException("could not branch to invalid value: " + label)
        }
      }
            
      case LoadInstruction(_, address) => {
        tungsten.LoadInstruction(cName, convertValue(address))
      }

      case ReturnInstruction(value) => {
        tungsten.ReturnInstruction(cName, convertValue(value))
      }

      case StoreInstruction(value, address, _) => {
        tungsten.StoreInstruction(cName, convertValue(address), convertValue(value))
      }
    }
    cDefinitions += cName -> cInst
    cInst
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

  def convertValue(value: Value): tungsten.Value = {
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
      case DefinedValue(name, _) => tungsten.DefinedValue(makeChildSymbol(name))
    }
  }

  def makeChildSymbol(name: String): Symbol = {
    val fullName = (name :: parents).reverse
    new Symbol(fullName)
  }

  def makeUniqueChildSymbol(name: String): Symbol = {
    val fullName = (name :: parents).reverse
    symbolFactory.complexSymbol(fullName)
  }

  def makeAnonymousChildSymbol: Symbol = makeUniqueChildSymbol("anon$")
}

class LlvmConversionException(msg: String) extends Exception(msg)
