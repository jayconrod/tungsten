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

object Assembler extends Converter[Module, Module] 
{
  def usageSynopsis = "assembler file1 file2..."

  override def availableOptions: List[CommandLineOption] = {
    super.availableOptions ++
      List(CommandLineOption(Some('r'), "no-runtime", "skips linking runtime definitions"))
  }

  def readSource(filename: String, input: InputStream): Option[Module] = {
    val code = readContentsOfFile(input)
    ModuleIO.parse(code, filename) match {
      case Left(module) => Some(module)
      case Right(errors) => {
        System.err.println("%s: parse errors occurred\n".format(filename))
        errors.foreach(System.err.println _)
        setErrorOccurred
        None
      }
    }
  }

  def writeTarget(target: Module, filename: String, output: OutputStream) {
    ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[Module] = {
    val linkedModule = if (hasOption("no-runtime"))
      source
    else
      linkRuntime(source)
    val errors = linkedModule.validate
    if (errors.isEmpty)
      Some(linkedModule)
    else {
      val filename = linkedModule.filename.map(_.toString + ": ").getOrElse("")
      System.err.println(filename + "validation errors occurred\n")
      errors.foreach(System.err.println _)
      setErrorOccurred
      None
    }
  }

  def convertFile(sourceFile: File): File = fileWithExtension(sourceFile, ".w", ".wo")
}

