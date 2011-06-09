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

import java.io._
import tungsten.ModuleIO
import tungsten.Utilities._

object LlvmToTungsten extends tungsten.Converter[Module, tungsten.Module] {
  def usageSynopsis = "llvm-to-tungsten file1 file2..."

  def readSource(filename: String, input: InputStream): Option[Module] = {
    val text = readContentsOfFile(input)
    val reader = new Lexer.Scanner(text)
    Parser.phrase(Parser.module)(reader) match {
      case Parser.Success(source, _) => Some(source)
      case error: Parser.NoSuccess => throw new IOException(error.msg)
    }
  }

  def writeTarget(target: tungsten.Module, filename: String, output: OutputStream) {
    ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[tungsten.Module] = {
    val target = LlvmToTungstenConverter(source)
    target.validate match {
      case Nil => Some(target)
      case errors => {
        errors.foreach(System.err.println _)
        None
      }
    }
  }

  def convertFile(sourceFilename: File): File = {
    fileWithExtension(sourceFilename, ".ll", ".wo")
  }
}
