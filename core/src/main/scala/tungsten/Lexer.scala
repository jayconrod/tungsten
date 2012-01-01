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

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharArrayReader
import CharArrayReader.EofCh
import scala.util.matching.Regex
import Utilities._

object Lexer extends Lexical with RegexParsers {
  override type Elem = Char

  val reservedStrings = {
    Set("{", "}", "(", ")", "[", "]", "->",
        "*", "?", "x", ",", "=", ":", "to", "<:", ">:",
        "/", "%", "+", "-", "<<", ">>", ">>>", "&", "^", "|",
        "<", "<=", ">", ">=", "==", "!=", "...",
        "exit",
        "name", "type", "intermediate", "library", "program", "version", "filename",
          "dependencies", "searchpaths", "is64bit", "safe",
        "annotation", "block", "class", "interface", "field", "function", "global", "struct",
          "constructors", "methods",
        "catch",
        "address", "binop", "bitcast", "branch", "cond", "extract", "fextend", "ftoi", 
          "ftruncate", "heap", "heaparray", "insert", "instanceof", "isextend", "itof", 
          "itruncate", "izextend",  "intrinsic", "load", "loadelement", "new", "nullcheck", 
          "pcall", "relop", "return", "scall", "stack", "stackarray", "store", "storeelement", 
          "throw", "unreachable", "upcast", "vcall", "vlookup",
        "unit", "boolean", "char", "string", "int8", "int16", "int32", "int64", "float32",
          "float64", "nulltype", "nothing", "struct",
        "true", "false", "null")
  }                            

  override lazy val whitespaceChar: Parser[Elem] = {
    elem(' ') | elem('\t') | elem('\n') | elem('\r')
  }

  lazy val comment: Parser[Any] = elem(';') ~ rep(chrExcept('\n', EofCh))

  lazy val whitespace: Parser[Any] = rep(whitespaceChar | comment)

  override def errorToken(message: String): Token = {
    ErrorTok(message)
  }

  lazy val unsignedInteger: Parser[Long] = {
    rep1(digit) ^^ { _.mkString.toLong }
  }      

  lazy val integer: Parser[Long] = {
    val binRegex = "0b([01]+)".r
    val decRegex = "(-?)([1-9]\\d*|0)".r
    val hexRegex = "0x([A-Fa-f0-9]+)".r

    def parseLong(str: String, base: Int): Long = {
      new java.math.BigInteger(str, base).longValue
    }

    val bin = binRegex ^^ { case binRegex(digits) => parseLong(digits, 2) }
    val dec = decRegex ^^ { case decRegex(sign, digits) => parseLong(sign + digits, 10) }
    val hex = hexRegex ^^ { case hexRegex(digits) => parseLong(digits, 16) }

    bin | hex | dec
  }

  lazy val float: Parser[Double] = {
    val optSign = opt(elem('-') | elem('+')) ^^ { c => c.map(_.toString).getOrElse("") }
    val num = rep1(digit) ^^ { _.mkString }
    val optNum = opt(num) ^^ { _.getOrElse("") }
    val exp = (elem('e') | elem('E')) ~ optSign ~ num ^^ {
      case e ~ s ~ n => e + s + n
    }
    val optExp = opt(exp) ^^ { _.getOrElse("") }

    val float1 = optSign ~ (num <~ '.') ~ optNum ~ optExp ^^ {
      case s ~ n ~ f ~ e => s + n + '.' + f + e
    }
    val float2 = (optSign <~ '.') ~ num ~ optExp ^^ {
      case s ~ f ~ e => s + '.' + f + e
    }
    val float3 = optSign ~ num ~ exp ^^ {
      case s ~ n ~ e => s + n + e
    }
    (float1 | float2 | float3) ^^ { _.toDouble }
  }

  def char(except: Set[Char] = Set()): Parser[Char] = {
    def isPrintable(c: Char): Boolean = charIsPrintable(c) && !except(c) && c != '\\'
    val printableChar =  elem("printable character", isPrintable _)
    val escapedChar = regex("\\\\[0-9A-Fa-f]{2}"r) ^^ { (s: String) =>
      val digits = s.substring(1)
      val code = (0 /: digits) { (code, digit) => 
        val digitValue = if ('0' <= digit && digit <= '9')
          digit - '0'
        else if ('A' <= digit && digit <= 'F')
          digit - 'A' + 10
        else
          digit - 'a' + 10
        code * 16 + digitValue
      }
      code.toChar
    }
    printableChar | escapedChar
  }

  lazy val quotedChar: Parser[Char] = {
    elem('\'') ~> char(Set('\'')) <~ elem('\'')
  }

  lazy val quotedString: Parser[String] = {
    elem('"') ~> rep(char(Set('"'))) <~ elem('"') ^^ { case s => s.mkString }
  }

  lazy val bareSymbol: Parser[Symbol] = {
    val element = regex("[A-Za-z_$][A-Za-z0-9_$]*"r) | quotedString
    val id = opt(elem('#') ~> integer) ^^ { case i => i.map(_.toInt).getOrElse(0) }
    rep1sep(element, '.') ~ id ^^ { 
      case es ~ i => Symbol(es, i)
    }
  }    

  /** This is not directly used by the lexer. Instead, Utilities.symbolFromString uses it
   *  to create symbols concisely in the compiler source code (mainly in tests).
   */
  def generalSymbol(prefixOptional: Boolean): Parser[Symbol] = {
    val prefix: Parser[String] = {
      if (prefixOptional)
        opt(elem('@') | elem('%')) ^^ { _.map(_.toString).getOrElse("") }
      else
        (elem('@') | elem('%')) ^^ { _.toString }
    }
    prefix ~ bareSymbol ^^ {
      case p ~ sym => {
        val name = sym.name
        val prefixedName = p + name.head :: name.tail
        Symbol(prefixedName, sym.id)
      }
    }
  }

  lazy val symbol: Parser[Symbol] = generalSymbol(false)

  lazy val reserved: Parser[String] = {
    def parseReserved(r: String): Parser[String] = {
      accept(r.toList) ^^ { _.mkString }
    }
    val reservedArray = new Array[String](reservedStrings.size)
    reservedStrings.copyToArray(reservedArray, 0)
    scala.util.Sorting.quickSort(reservedArray)
    val reservedParsers = reservedArray.toList.map(parseReserved _)
    val fail: Parser[String] = failure("no matching reserved string")
    (fail /: reservedParsers) { (res, p) => p | res }
  } 

  lazy val bareVersion: Parser[Version] = {
    rep1sep(unsignedInteger, ".") ^^ { vs => Version(vs.map(_.toInt)) }
  }

  lazy val version: Parser[Version] = {
    elem('v') ~> bareVersion
  }

  lazy val moduleDependency: Parser[ModuleDependency] = {
    val versionRange: Parser[(Version, Version)] = {
      ((bareVersion <~ elem('-')) ~ opt(bareVersion) ^^ {
        case vmin ~ vmax => (vmin, vmax.getOrElse(Version.MAX))
      }) |
      (elem('-') ~> bareVersion) ^^ {
        case vmax => (Version.MIN, vmax)
      }
    }
    elem('-') ~> elem('l') ~> bareSymbol ~ opt(elem(':') ~> versionRange) ^^ {
      case name ~ None => ModuleDependency(name, Version.MIN, Version.MAX)
      case name ~ Some((vmin, vmax)) => ModuleDependency(name, vmin, vmax)
    }
  }

  def token: Parser[Token] = {
    (version          ^^ { v => VersionTok(v) })          |
    (moduleDependency ^^ { v => ModuleDependencyTok(v) }) |
    (float            ^^ { v => FloatTok(v) })            |
    (integer          ^^ { v => IntTok(v) })              |
    (quotedChar       ^^ { v => CharTok(v) })             |
    (quotedString     ^^ { v => StringTok(v) })           |
    (symbol           ^^ { v => SymbolTok(v) })           |
    (reserved         ^^ { v => ReservedTok(v) })
  }

  def test[T](input: String, parser: Parser[T]): T = {
    val reader = new CharArrayReader(input.toArray)
    phrase(parser)(reader).get
  }

  def test(input: String): Token = test(input, token)
}
