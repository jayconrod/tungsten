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
import scala.util.parsing.input.CharSequenceReader
import tungsten.Utilities._

object test {
  def main(args: Array[String]) {
    if (args.size != 1) {
      System.err.println("usage: test file.ll")
      System.exit(1)
    }

    val filename = args(0)
    val file = new File(filename)
    val text = readContentsOfFile(file)
    val reader = new CharSequenceReader(text)
    val scanner = new Lexer.Scanner(reader)
    val module = Parser.phrase(Parser.module)(scanner) match {
      case Parser.Success(ast, _) => Left(ast)
      case parseError: Parser.NoSuccess => {
        System.err.println("parse error: " + parseError.msg)
        System.exit(1)
        null
      }
    }
    System.out.println(module)
  }
}
