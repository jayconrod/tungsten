package tungsten.llvm

import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverter(module: tungsten.Module) {
  def convert: Module = {
    throw new UnsupportedOperationException
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
