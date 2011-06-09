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

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input._
import scala.util.matching.Regex
import Utilities._

object Lexer extends Lexical with RegexParsers {
  override type Elem = Char

  val reservedOperators = Set("=", ":", "{", "}", "(", ")", "[", "]", "*", ",")
  val reservedWords = Set("datalayout", "define", "nounwind", "target", "triple", "to",
                          "align", "label", "void", "float", "double",
                          "add", "alloca", "and", "asr", "bitcast", "br", "extractvalue", 
                            "fadd", "fcmp", "fdiv", "fmul", "fpext", "fptosi", "fptoui", "frem", 
                            "fsub", "fptrunc", "getelementptr", "icmp", "insertvalue", 
                            "inttoptr", "load", "lsr", "mul", "or", "phi", "ptrtoint", "ret", 
                            "sdiv", "sext", "shl", "sitofp", "srem", "store", "sub", "trunc", 
                            "uitofp", "unreachable", "udiv", "urem", "xor", "zext",
                          "false", "oeq", "ogt", "oge", "olt", "ole", "ord", "ueq", "ugt", "uge",
                            "ult", "ule", "une", "uno", "true", "eq", "ne", "sgt", "sge", "slt", 
                            "sle",
                          "zeroext", "signext", "inreg", "byval", "sret", "noalias", 
                            "nocapture", "nest",
                          "alwaysinline", "inlinehint", "optsize", "noreturn", "nounwind",
                            "readnone", "readonly", "ssp", "sspreq", "noredzone", 
                            "noimplicithint", "naked")

  override def whitespaceChar: Parser[Elem] = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  def comment: Parser[Any] = elem(';') ~ rep(chrExcept('\n')) ~ elem('\n')

  def whitespace: Parser[Any] = rep(whitespaceChar | comment)

  override def errorToken(message: String): Token = {
    ErrorToken(message)
  }

  def word: Parser[Token] = {
    def checkReserved(s: String): Parser[Token] = {
      if (reservedWords(s))
        success(ReservedToken(s))
      else
        elem(':') ^^^ LabelToken(s)
    }
    regex(new Regex("[A-Za-z._$][A-Za-z0-9._$]*")) >> checkReserved
  }

  def operator: Parser[Token] = {
    def parseOperator(r: String): Parser[ReservedToken] = {
      accept(r.toList) ^^ { s => ReservedToken(s.mkString) }
    }
    val reservedArray = new Array[String](reservedOperators.size)
    reservedOperators.copyToArray(reservedArray, 0)
    scala.util.Sorting.quickSort(reservedArray)
    val reservedParsers = reservedArray.toList.map(parseOperator _)
    val fail: Parser[ReservedToken] = failure("no matching reserved string")
    (fail /: reservedParsers) {(x, y) => y | x}
  }

  def hexChar: Parser[Char] = {
    elem("hex character", { c => ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') })
  }  

  def stringChar: Parser[List[Char]] = {
    (elem('\\') ~ repN(2, hexChar) ^^ { case bs ~ hex => bs :: hex }) |
    (chrExcept('"', '\n') ^^ { (c: Char) => List(c) })
  }

  def string: Parser[StringToken] = {
    elem('"') ~ rep(stringChar) ~ elem('"') ^^ { 
      case q1 ~ s ~ q2 => StringToken(q1 + s.flatten.mkString + q2)
    }
  }

  def integer: Parser[IntToken] = {
    opt('-') ~ rep1(digit) ^^ { 
      case sign ~ digits => {
        val signStr = sign.map(_.toString).getOrElse("")
        IntToken(signStr + digits.mkString)
      }
    }
  }

  def float: Parser[FloatToken] = {
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
    (float1 | float2 | float3) ^^ { case s => FloatToken(s) }
  }

  def intType: Parser[IntTypeToken] = {
    elem('i') ~ rep1(digit) ^^ { case i ~ n => IntTypeToken(i + n.mkString) }
  }

  def symbol: Parser[SymbolToken] = {
    def normalSymbol = identifier
    def quotedSymbol = elem('"') ~ rep1(chrExcept('"')) ~ elem('"') ^^ {
      case q1 ~ s ~ q2 => q1 + s.mkString + q2
    }
    def numericSymbol = rep1(digit) ^^ { _.mkString }
    (elem('%') | elem('@')) ~ (normalSymbol | quotedSymbol | numericSymbol) ^^ { 
      case prefix ~ sym => SymbolToken(prefix + sym) }
  }

  def label: Parser[LabelToken] = {
    (identifier <~ ':' ^^ { case s => LabelToken(s) }) |
    (string <~ ':' ^^ { case s => LabelToken(s.value) })
  }

  def identifier: Parser[String] = regex(identifierRegex)

  def token: Parser[Token] = {
    operator |
    word     |
    label    |
    string   |
    float    |
    integer  |
    intType  |
    symbol
  }

  def test(input: String) = {
    val reader = new CharArrayReader(input.toArray)
    phrase(token)(reader) match {
      case Success(tok, _) => tok
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}
