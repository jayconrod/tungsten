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

trait Converter[S, T] extends CommandLineProgram {
  private var errorOccurred = false

  override def availableOptions: List[CommandLineOption] = {
    super.availableOptions ++
      List(CommandLineOption(Some('o'), "output", "write output to a specific file", Some("filename")))
  }

  override def main(args: Array[String]) {
    super.main(args)

    if (optionValues.contains("output") && arguments.size != 1) {
      System.err.println("error: if output file is specified, exactly one input file must be given")
      System.exit(1)
    }

    if (arguments.isEmpty) {
      for (source <- readSourceFromStdin;
           target <- convert(source))
        writeTargetToStdout(target)
    } else {
      for (filename <- arguments;
           val inputFile =  new File(filename);
           source <- readSourceFromFile(inputFile);
           target <- convert(source))
      {
        val outputFile = optionValues.get("output") match {
          case Some(filename) => new File(filename)
          case None => convertFile(inputFile)
        }
        writeTargetToFile(target, outputFile)
      }
    }

    if (errorOccurred)
      System.exit(FAILURE_CODE)
  }

  protected def setErrorOccurred {
    errorOccurred = true
  }

  private final def readSourceFromStdin: Option[S] = {
    val filename = "<stdin>"
    handleInputErrors(filename) {
      readSource(filename, System.in)
    }
  }

  private final def readSourceFromFile(file: File): Option[S] = {
    val filename = file.getName
    handleInputErrors(filename) {
      val input = new BufferedInputStream(new FileInputStream(file))
      readSource(filename, input)
    }
  }      

  def readSource(filename: String, input: InputStream): Option[S]

  private final def writeTargetToStdout(target: T) {
    val filename = "<stdout>"
    handleOutputErrors(filename) {
      writeTarget(target, filename, System.out)
    }
  }

  private final def writeTargetToFile(target: T, file: File) {
    val filename = file.getName
    var output: OutputStream = null
    handleOutputErrors(filename) {
      output = new BufferedOutputStream(new FileOutputStream(file))
      writeTarget(target, filename, output)
    }
    if (errorOccurred && output != null) {
      output.close
      file.delete
    }
  }

  private final def handleInputErrors(filename: String)(code: => Option[S]): Option[S] = {
    try {
      code
    } catch {
      case exn: IOException => {
        System.err.println("%s: IO Error: %s".format(filename, exn.getMessage))
        setErrorOccurred
        None
      }
    }
  }

  private final def handleOutputErrors(filename: String)(code: => Unit) {
    try {
      code
    } catch {
      case exn: IOException => {
        System.err.println("%s: IO Error: %s".format(filename, exn.getMessage))
        setErrorOccurred
      }
    }
  }        

  def writeTarget(target: T, filename: String, output: OutputStream)

  def convert(source: S): Option[T]

  def convertFile(sourceFile: File): File
}
