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

import java.io.{File, InputStream, OutputStream}

object Opt extends Converter[Module, Module]
{
  private var passes = Map[String, Pass]()

  def registerPass(pass: Pass) {
    if (passes.contains(pass.name)) {
      System.err.println("error: duplicate pass registered: " + pass.name)
      System.exit(1)
    }
    passes += pass.name -> pass
  }
  registerPass(new LowerPass)

  def usageSynopsis = "opt [-pass1 -pass2...] file1 file2..."

  override def availableOptions: List[CommandLineOption] = {
    val options = passes.values.map { pass => 
      CommandLineOption(None, pass.name, pass.description)
    }
    super.availableOptions ++ options
  }

  def readSource(filename: String, input: InputStream): Option[Module] = {
    val module = ModuleIO.readBinary(input)
    val errors = module.validate
    if (errors.isEmpty)
      Some(module)
    else {
      System.err.println(filename + ": validation errors occurred")
      errors.foreach(System.err.println _)
      System.err.println()
      setErrorOccurred
      None
    }
  }

  def writeTarget(target: Module, filename: String, output: OutputStream) {
    ModuleIO.writeBinary(target, output)
  }

  def convert(source: Module): Option[Module] = {
    var m = source
    for (passName <- options;
         pass     <- passes.get(passName))
      m = pass(m)
    Some(m)
  }

  def convertFile(sourceFile: File): File = sourceFile
}
