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
import scala.collection.mutable._
import scala.util.parsing.input.CharSequenceReader
import Utilities._

object ModuleIO {
  def readBinary(file: File): Module = {
    var input: InputStream = null
    try {
      input = new BufferedInputStream(new FileInputStream(file))
      readBinary(input)
    } finally {
      if (input != null)
        input.close
    }
  }

  def readBinary(input: InputStream): Module = {
    val in = new DataInputStream(input)
    val reader = new BinaryModuleReader(in)
    reader.read
  }

  def readText(file: File): Module = {
    val input = new BufferedReader(new FileReader(file))
    val module = readText(input, file.getName)
    input.close
    module
  }

  def readText(input: Reader): Module = {
    readText(input, "<STDIN>")
  }    

  def readText(input: Reader, filename: String): Module =
  {
    val text = readContentsOfFile(input)
    readText(text, filename)
  }

  def readText(text: String,
               filename: String = "<STDIN>"): Module =
  {
    parse(text, filename) match {
      case Left(module) => module
      case Right(errors) => throw new IOException(errors.head)
    }
  }

  def writeBinary(module: Module, file: File) {
    val output = new BufferedOutputStream(new FileOutputStream(file))
    writeBinary(module, output)
    output.close
  }

  def writeBinary(module: Module, output: OutputStream) {
    val out = new DataOutputStream(output)
    val writer = new BinaryModuleWriter(module, out)
    writer.write
    out.flush
  }

  def writeText(module: Module, file: File) {
    val output = new BufferedWriter(new FileWriter(file))
    writeText(module, output)
    output.close
  }

  def writeText(module: Module, output: Writer) {
    val writer = new TextModuleWriter(module, output)
    writer.write
    output.flush
  }

  /* readText helpers */
  def parse(text: String, filename: String): Either[Module, List[CompileException]] = {
    val file = new File(filename)
    val reader = new CharSequenceReader(text)
    val scanner = new Lexer.Scanner(reader)
    val parser = new Parser
    parser.phrase(parser.module(file))(scanner) match {
      case parser.Success((headers, asts), _) => {
        val definitions = asts.map(_.toList).flatten
        var module = headers
        var errors: List[CompileException] = Nil
        for (defn <- definitions) {
          if (module.definitions.contains(defn.name))
            errors ::= RedefinedSymbolException(defn.name, defn.getLocation, module.definitions(defn.name).getLocation)
          else
            module = module.add(defn)
        }
        if (errors.isEmpty)
          Left(module)
        else
          Right(errors)
      }
      case error: parser.NoSuccess => Right(List(ParseException(error.msg, Nowhere)))
    }
  }            
}
