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

package tungsten

import java.io._
import Utilities._

object Disassembler extends Converter[Module, Module] {
  def usageSynopsis = "disassembler file1 file2..."

  def readSource(filename: String, input: InputStream): Option[Module] = {
    Some(ModuleIO.readBinary(input))
  }

  def writeTarget(target: Module, filename: String, output: OutputStream) {
    val writer = new OutputStreamWriter(output)
    ModuleIO.writeText(target, writer)
  }

  def convert(source: Module): Option[Module] = Some(source)

  def convertFile(sourceFile: File): File = fileWithExtension(sourceFile, ".wo", ".w")
}
