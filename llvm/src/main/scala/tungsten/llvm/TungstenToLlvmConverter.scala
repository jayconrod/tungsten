package tungsten.llvm

import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverter(module: tungsten.Module) {
  def convert: Module = {
    throw new UnsupportedOperationException
  }

  def globalSymbol(symbol: Symbol): String = {
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
      case Some(m) if m == symbolStr => "@" + symbolStr
      case _ => "@\"%s\"".format(symbolStr)
    }
  }
}

object TungstenToLlvmConverter {
  def apply(module: tungsten.Module): Module = {
    new TungstenToLlvmConverter(module).convert
  }
}
