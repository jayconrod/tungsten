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
