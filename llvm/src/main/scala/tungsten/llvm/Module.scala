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
    val buffer = new StringBuilder(dataLayoutStr + tripleStr + "\n")

    definitions.values.collect { case s: Struct => s }.foreach { s => buffer.append(s + "\n\n") }
    definitions.values.collect { case g: Global => g }.foreach { g => buffer.append(g + "\n\n") }
    definitions.values.collect { case f: Function => f }.foreach { f => buffer.append(f + "\n\n") }
    
    buffer.append("\n")
    buffer.toString
  }
}
