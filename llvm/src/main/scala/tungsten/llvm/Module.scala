package tungsten.llvm

import tungsten.Utilities._
import Utilities._

class Module(val targetDataLayout: Option[String],
             val targetTriple: Option[String],
             val definitions: Map[String, Definition])
{
  override def equals(that: Any): Boolean = {
    that match {
      case m: Module if targetDataLayout == m.targetDataLayout &&
                        targetTriple     == m.targetTriple     &&
                        definitions      == m.definitions => true
      case _ => false
    }
  }

  override def hashCode: Int = hash("Module", targetDataLayout, targetTriple, definitions)

  override def toString = {
    val dataLayoutStr = targetDataLayout match {
      case Some(dl) => "target datalayout = %s\n".format(escapeString(dl))
      case None     => ""
    }
    val tripleStr = targetTriple match {
      case Some(t) => "target triple = %s\n".format(escapeString(t))
      case None    => ""
    }
    val buffer = new StringBuffer(dataLayoutStr + tripleStr + "\n")
    for (defn <- definitions.valuesIterator)
      buffer.append(defn.toString)
    buffer.append("\n")
    buffer.toString
  }
}
