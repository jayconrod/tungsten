package tungsten.llvm

import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverter(module: tungsten.Module) {
  def convert: Module = {
    throw new UnsupportedOperationException
  }

  def convertInstruction(instruction: tungsten.Instruction, parent: Symbol): Instruction = {
    val localName = localSymbol(instruction.name, parent)
    instruction match {
      case tungsten.AddressInstruction(_, _, base, indices, _) => {
        GetElementPointerInstruction(localName, 
                                     convertValue(base, parent), 
                                     indices.map(convert32BitValue(_, parent)))
      }
      case _ => throw new UnsupportedOperationException // TODO
    }
  }

  def convertValue(value: tungsten.Value, parent: Symbol): Value = {
    value match {
      case tungsten.UnitValue => VoidValue
      case tungsten.BooleanValue(true) => IntValue(1L, 1)
      case tungsten.BooleanValue(false) => IntValue(0L, 1)
      case tungsten.IntValue(value, width) => IntValue(value, width)
      case tungsten.FloatValue(value, width) => FloatValue(value, width)
      case tungsten.NullValue => NullValue(IntType(8))
      case tungsten.ArrayValue(ety, elements) => {
        ArrayValue(convertType(ety), elements.map(convertValue(_, parent)))
      }
      case tungsten.StructValue(_, elements) => {
        StructValue(elements.map(convertValue(_, parent)))
      }
      case tungsten.DefinedValue(name, ty) => {
        DefinedValue(localSymbol(name, parent), convertType(ty))
      }
      case _ => throw new UnsupportedOperationException
    }
  }

  def convert32BitValue(value: tungsten.Value, parent: Symbol): Value = {
    value match {
      case tungsten.IntValue(v, _) => IntValue(v, 32)
      case tungsten.DefinedValue(name, _: tungsten.IntType) => {
        DefinedValue(localSymbol(name, parent), IntType(32))
      }
      case _ => convertValue(value, parent)
    }
  }

  def convertType(ty: tungsten.Type): Type = {
    ty match {
      case tungsten.UnitType => VoidType
      case tungsten.BooleanType => IntType(1)
      case tungsten.IntType(width) => IntType(width)
      case tungsten.FloatType(width) => FloatType(width)
      case tungsten.PointerType(tungsten.UnitType) => PointerType(IntType(8))
      case tungsten.PointerType(ety) => PointerType(convertType(ety))
      case tungsten.NullType => PointerType(IntType(8))
      case tungsten.ArrayType(None, ety) => ArrayType(0L, convertType(ety))
      case tungsten.ArrayType(Some(size), ety) => ArrayType(size, convertType(ety))
      case tungsten.StructType(structName) => {
        val globalName = globalSymbol(structName)
        val localName = "%" + globalName.tail
        NamedStructType(localName)
      }
      case _ => throw new UnsupportedOperationException
    }
  }

  def globalSymbol(symbol: Symbol): String = {
    "@" + convertSymbol(symbol)
  }

  def localSymbol(symbol: Symbol, parent: Symbol): String = {
    def findChildName(name: List[String], parentName: List[String]): List[String] = {
      (name, parentName) match {
        case (nameHd :: nameTl, parentHd :: parentTl) if nameHd == parentHd => {
          findChildName(nameTl, parentTl)
        }
        case (Nil, _) => symbol.name
        case _ => name
      }
    }

    val childSymbol = new Symbol(findChildName(symbol.name, parent.name), symbol.id)
    "%" + convertSymbol(childSymbol)
  }    

  def convertSymbol(symbol: Symbol): String = {
    def convertNamePart(part: String): String = {
      val buffer = new StringBuffer
      for (c <- part) {
        if (c.toInt < 0x80 && !Character.isISOControl(c) && c != '"' && c != '\\')
          buffer.append(c)
        else if (c.toInt < 0xFF)
          buffer.append("\\%02x".format(c.toInt))
        else
          buffer.append("\\%02x\\%02x".format(c.toInt >>> 8, c.toInt & 0xFF))
      }
      buffer.toString
    }
    
    val idStr = if (symbol.id != 0) "." + symbol.id else ""
    val nameStr = symbol.name.map(convertNamePart _).mkString(".")
    val symbolStr = nameStr + idStr
    val identRegex = "[a-zA-Z$._][a-zA-Z$._0-9]*".r
    identRegex.findFirstIn(symbolStr) match {
      case Some(m) if m == symbolStr => symbolStr
      case _ => "\"" + symbolStr + "\""
    }
  }    
}

object TungstenToLlvmConverter {
  def apply(module: tungsten.Module): Module = {
    new TungstenToLlvmConverter(module).convert
  }
}
