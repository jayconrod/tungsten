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

  override def main(args: Array[String]) {
    super.main(args)

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
        val outputFile = convertFile(inputFile)
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
    readSourceWrapper("<stdin>", System.in)
  }

  private final def readSourceFromFile(file: File): Option[S] = {
    try {      
      val input = new BufferedInputStream(new FileInputStream(file))
      val source = readSourceWrapper(file.getName, input)
      input.close
      source
    } catch {
      case exn: IOException => {
        System.err.println(file + ": IO error: " + exn.getMessage)
        errorOccurred
        None
      }
    }
  }

  private final def readSourceWrapper(filename: String, input: InputStream): Option[S] = {
    try {
      val source = readSource(filename, input)
      if (!source.isDefined)
        errorOccurred = true
      source
    } catch {
      case exn: IOException => {
        System.err.println(filename + ": IO error: " + exn.getMessage)
        errorOccurred = true
        None
      }
    }
  }

  def readSource(filename: String, input: InputStream): Option[S]

  private final def writeTargetToStdout(target: T) {
    writeTargetWrapper(target, "<stdout>", System.out)
  }

  private final def writeTargetToFile(target: T, file: File) {
    val output = new BufferedOutputStream(new FileOutputStream(file))
    writeTargetWrapper(target, file.getName, output)
    output.close
  }

  private final def writeTargetWrapper(target: T, filename: String, output: OutputStream) {
    try {
      writeTarget(target, filename, output)
    } catch {
      case exn: IOException => {
        System.err.println(filename + ": IO error: " + exn.getMessage)
        errorOccurred = true
      }
    }
  }

  def writeTarget(target: T, filename: String, output: OutputStream)

  def convert(source: S): Option[T]

  def convertFile(sourceFile: File): File
}
