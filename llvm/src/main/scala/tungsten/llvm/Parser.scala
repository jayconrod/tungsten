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
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

object Parser extends Parsers with ImplicitConversions {
  type Elem = AnyRef

  implicit def reserved(r: String): Parser[String] = elem(ReservedToken(r)) ^^^ r

  def module: Parser[Module] = {
    targetDataLayout ~ targetTriple ~ rep(definition) ^^ {
      case tdl ~ tt ~ ds => {
        val defnMap = (Map[String, Definition]() /: ds) { (map, defn) =>
          map + (defn.name -> defn)
        }
        new Module(tdl, tt, defnMap)
      }
    }
  }

  def targetDataLayout: Parser[Option[String]] = {
    opt("target" ~> "datalayout" ~> "=" ~> string)
  }

  def targetTriple: Parser[Option[String]] = {
    opt("target" ~> "triple" ~> "=" ~> string)
  }

  def definition: Parser[Definition] = {
    function
  }

  def function: Parser[Function] = {
    "define" ~> parameterAttributes ~ ty ~ globalSymbol ~ 
      ("(" ~> repsep(defnParameter, ",") <~ ")") ~ functionAttributes ~
      ("{" ~> rep1(block) <~ "}") ^^ {
      case ras ~ rty ~ n ~ ps ~ fas ~ bs => Function(n, ras, rty, ps, false, fas, bs)
    }
  }

  def defnParameter: Parser[Parameter] = {
    ty ~ parameterAttributes ~ localSymbol ^^ { case t ~ as ~ n => Parameter(n, t, as) }
  }

  def functionAttributes: Parser[Set[FunctionAttribute]] = {
    rep(functionAttribute) ^^ { _.toSet }
  }

  def functionAttribute: Parser[FunctionAttribute] = {
    import FunctionAttribute._
    ("alwaysinline" ^^^ ALWAYSINLINE)       |
    ("inlinehint" ^^^ INLINEHINT)           |
    ("optsize" ^^^ OPTSIZE)                 |
    ("noreturn" ^^^ NORETURN)               |
    ("nounwind" ^^^ NOUNWIND)               |
    ("readnone" ^^^ READNONE)               |
    ("readonly" ^^^ READONLY)               |
    ("ssp" ^^^ SSP)                         |
    ("sspreq" ^^^ SSPREQ)                   |
    ("noredzone" ^^^ NOREDZONE)             |
    ("noimplicitfloat" ^^^ NOIMPLICITFLOAT) |
    ("naked" ^^^ NAKED)
  }

  def parameterAttributes: Parser[Set[ParameterAttribute]] = {
    rep(parameterAttribute) ^^ { _.toSet }
  }

  def parameterAttribute: Parser[ParameterAttribute] = {
    import ParameterAttribute._
    ("zeroext" ^^^ ZEROEXT)     |
    ("signext" ^^^ SIGNEXT)     |
    ("inreg" ^^^ INREG)         |
    ("byval" ^^^ BYVAL)         |
    ("sret" ^^^ SRET)           |
    ("noalias" ^^^ NOALIAS)     |
    ("nocapture" ^^^ NOCAPTURE) |
    ("nest" ^^^ NEST)
  }

  def block: Parser[Block] = {
    label ~ rep1(instruction) ^^ {
      case n ~ is => Block(n, is)
    }
  }

  def instruction: Parser[Instruction] = {
    addInst          |
    allocaInst       |
    andInst          |
    asrInst          |
    bitcastInst      |
    branchInst       |
    extractvalueInst |
    faddInst         |
    fcmpInst         |
    fdivInst         |
    fmulInst         |
    fpextInst        |
    fptosiInst       |
    fptouiInst       |
    fptruncInst      |
    fremInst         |
    fsubInst         |
    gepInst          |
    icmpInst         |
    insertvalueInst  |
    inttoptrInst     |
    loadInst         |
    lsrInst          |
    mulInst          |
    orInst           |
    phiInst          |
    ptrtointInst     |
    retInst          |
    sdivInst         |
    sextInst         |
    shlInst          |
    sitofpInst       |
    sremInst         |
    storeInst        |
    subInst          |
    truncInst        |
    uitofpInst       |
    unreachableInst  |
    udivInst         |
    uremInst         |
    xorInst          |
    zextInst
  }

  def binopInst(opname: String, 
                constructor: Function4[String, Type, Value, Value, Instruction]): Parser[Instruction] =
  {
    (localSymbol <~ "=" <~ opname) ~ ty >> { 
      case n ~ t => (untypedValue(t) <~ ",") ~ untypedValue(t) ^^ {
        case l ~ r => constructor(n, t, l, r)
      }
    }
  }

  def conversionInst(opname: String,
                     constructor: Function3[String, Value, Type, Instruction]): Parser[Instruction] =
  {
    (localSymbol <~ "=" <~ opname) ~ (value <~ "to") ~ ty ^^ {
      case n ~ v ~ t => constructor(n, v, t)
    }
  }

  def comparison: Parser[Comparison] = {
    import Comparison._
    ("false" ^^^ FALSE) |
    ("oeq"   ^^^ OEQ)   |
    ("ogt"   ^^^ OGT)   |
    ("oge"   ^^^ OGE)   |
    ("olt"   ^^^ OLT)   |
    ("ole"   ^^^ OLE)   |
    ("one"   ^^^ ONE)   |
    ("ord"   ^^^ ORD)   |
    ("ueq"   ^^^ UEQ)   |
    ("ugt"   ^^^ UGT)   |
    ("uge"   ^^^ UGE)   |
    ("ult"   ^^^ ULT)   |
    ("ule"   ^^^ ULE)   |
    ("une"   ^^^ UNE)   |
    ("uno"   ^^^ UNO)   |
    ("true"  ^^^ TRUE)  |
    ("eq"    ^^^ EQ)    |
    ("ne"    ^^^ NE)    |
    ("sgt"   ^^^ SGT)   |
    ("sge"   ^^^ SGE)   |
    ("slt"   ^^^ SLT)   |
    ("sle"   ^^^ SLE)
  }

  def addInst = binopInst("add", AddInstruction.apply _)

  def allocaInst: Parser[Instruction] = {
    (localSymbol <~ "=" <~ "alloca") ~ ty ~ opt("," ~> value) ^^ {
      case n ~ t ~ Some(c) => AllocaArrayInstruction(n, t, c)
      case n ~ t ~ None => AllocaInstruction(n, t)
    }
  }

  def andInst = binopInst("and", AndInstruction.apply _)
  def asrInst = binopInst("asr", ArithmeticShiftRightInstruction.apply _)

  def bitcastInst: Parser[BitCastInstruction] = {
    (localSymbol <~ "=" <~ "bitcast") ~ (value <~ "to") ~ ty ^^ {
      case n ~ v ~ t => BitCastInstruction(n, v, t)
    }
  }

  def branchInst: Parser[BranchInstruction] = {
    ("br" ~> value) ^^ { case l => BranchInstruction(l) }
  }  

  def extractvalueInst: Parser[ExtractValueInstruction] = {
    (localSymbol <~ "=" <~ "extractvalue") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case n ~ b ~ is => ExtractValueInstruction(n, b, is)
    }
  }

  def faddInst = binopInst("fadd", FloatAddInstruction.apply _)

  def fcmpInst: Parser[FloatCompareInstruction] = {
    (localSymbol <~ "=" <~ "fcmp") ~ comparison ~ ty >> {
      case n ~ c ~ t => (untypedValue(t) <~ ",") ~ untypedValue(t) ^^ {
        case l ~ r => FloatCompareInstruction(n, c, t, l, r)
      }
    }
  }

  def fdivInst = binopInst("fdiv", FloatDivideInstruction.apply _)
  def fmulInst = binopInst("fmul", FloatMultiplyInstruction.apply _)
  def fpextInst = conversionInst("fpext", FloatExtendInstruction.apply _)
  def fptosiInst = conversionInst("fptosi", FloatToSignedIntegerInstruction.apply _)
  def fptouiInst = conversionInst("fptoui", FloatToUnsignedIntegerInstruction.apply _)
  def fptruncInst = conversionInst("fptrunc", FloatTruncateInstruction.apply _)
  def fremInst = binopInst("frem", FloatRemainderInstruction.apply _)
  def fsubInst = binopInst("fsub", FloatSubtractInstruction.apply _)

  def gepInst: Parser[GetElementPointerInstruction] = {
    (localSymbol <~ "=" <~ "getelementptr") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case n ~ b ~ is => GetElementPointerInstruction(n, b, is)
    }
  }

  def icmpInst: Parser[IntegerCompareInstruction] = {
    (localSymbol <~ "=" <~ "icmp") ~ comparison ~ ty >> {
      case n ~ c ~ t => (untypedValue(t) <~ ",") ~ untypedValue(t) ^^ {
        case l ~ r => IntegerCompareInstruction(n, c, t, l, r)
      }
    }
  }

  def insertvalueInst: Parser[InsertValueInstruction] = {
    (localSymbol <~ "=" <~ "insertvalue") ~ (value <~ ",") ~ (value <~ ",") ~
      rep1sep(value, ",") ^^ {
        case n ~ b ~ v ~ is => InsertValueInstruction(n, b, v, is)
      }
  }      

  def inttoptrInst = conversionInst("inttoptr", IntegerToPointerInstruction.apply _)

  def loadInst: Parser[LoadInstruction] = {
    (localSymbol <~ "=" <~ "load") ~ value ~ opt("," ~> alignment) ^^ {
      case n ~ p ~ a => LoadInstruction(n, p, a)
    }
  }

  def lsrInst = binopInst("lsr", LogicalShiftRightInstruction.apply _)
  def mulInst = binopInst("mul", MultiplyInstruction.apply _)
  def orInst = binopInst("or", OrInstruction.apply _)

  def phiInst: Parser[PhiInstruction] = {
    def phiEntries(intro: String ~ Type) = {
      val name ~ ty = intro
      rep1sep("[" ~> (untypedValue(ty) <~ ",") ~ localSymbol <~ "]", ",") ^^ { entries =>
        PhiInstruction(name, ty, entries.map { case v ~ l => (v, l) })
      }
    }
          
    ((localSymbol <~ "=" <~ "phi") ~ ty) >> phiEntries
  }

  def ptrtointInst = conversionInst("ptrtoint", PointerToIntegerInstruction.apply _)

  def retInst: Parser[ReturnInstruction] = {
    ("ret" ~> value) ^^ { case v => ReturnInstruction(v) }
  }

  def sdivInst = binopInst("sdiv", SignedDivideInstruction.apply _)
  def sextInst = conversionInst("sext", SignExtendInstruction.apply _)
  def shlInst = binopInst("shl", ShiftLeftInstruction.apply _)
  def sitofpInst = conversionInst("sitofp", SignedIntegerToFloatInstruction.apply _)
  def sremInst = binopInst("srem", SignedRemainderInstruction.apply _)

  def storeInst: Parser[StoreInstruction] = {
    "store" ~> (value <~ ",") ~ value ~ opt("," ~> alignment) ^^ {
      case v ~ p ~ a => StoreInstruction(v, p, a)
    }
  }

  def subInst = binopInst("sub", SubtractInstruction.apply _)
  def truncInst = conversionInst("trunc", TruncateInstruction.apply _)
  def uitofpInst = conversionInst("uitofp", UnsignedIntegerToFloatInstruction.apply _)

  def unreachableInst = {
    "unreachable" ^^^ UnreachableInstruction
  }

  def udivInst = binopInst("udiv", UnsignedDivideInstruction.apply _)
  def uremInst = binopInst("urem", UnsignedRemainderInstruction.apply _)
  def xorInst = binopInst("xor", XorInstruction.apply _)
  def zextInst = conversionInst("zext", ZeroExtendInstruction.apply _)

  def alignment: Parser[Int] = {
    "align" ~> integer ^^ { _.toInt }
  }

  def value: Parser[Value] = {
    ("void" ^^^ VoidValue) |
    (ty >> untypedValue)
  }

  def untypedValue(ty: Type): Parser[Value] = {
    acceptMatch("integer", { case t: IntToken if ty.isInstanceOf[IntType] => 
      IntValue(t.value, ty.asInstanceOf[IntType].width)
    }) |
    acceptMatch("float value", { case t: FloatToken if ty.isInstanceOf[FloatType] =>
      FloatValue(t.value, ty.asInstanceOf[FloatType].width)
    }) |
    structValue(ty) |
    ((localSymbol | globalSymbol) ^^ { case n => DefinedValue(n, ty) }) |
    bitcastValue(ty)
  }

  def structValue(ty: Type): Parser[Value] = {
    def untypedStructValue: Parser[List[Value]] = "{" ~> repsep(value, ",") <~ "}"
    ty match {
      case NamedStructType(name) => 
        untypedStructValue ^^ { case vs => NamedStructValue(name, vs) }
      case StructType(elementTypes) => 
        untypedStructValue ^^ { case vs => StructValue(elementTypes, vs) }
      case _ => failure("struct type must precede struct value")
    }
  }

  def bitcastValue(tyPrefix: Type): Parser[Value] = {
    "bitcast" ~> "(" ~> (value <~ "to") ~ ty <~ ")" ^? {
      case v ~ t if t == tyPrefix => BitCastValue(v, t)
    }
  }

  def ty: Parser[Type] = {
    def basicTy = {
      intType | 
      ("float" ^^^ FloatType(32)) |
      ("double" ^^^ FloatType(64)) |
      ("void" ^^^ VoidType) |
      ("label" ^^^ LabelType) |
      (localSymbol ^^ { case name => NamedStructType(name) }) |
      ("{" ~> repsep(ty, ",") <~ "}" ^^ { case elementTypes => StructType(elementTypes) })
    }
    def makePointer(elementType: Type, count: Int): Type = {
      if (count == 0)
        elementType
      else
        makePointer(PointerType(elementType), count - 1)
    }
    basicTy ~ rep("*") ^^ {
      case ety ~ stars => makePointer(ety, stars.size)
    }
  }

  def intType: Parser[IntType] = {
    accept("integer type", { case t: IntTypeToken => IntType(t.width) })
  }

  def string: Parser[String] = {
    accept("string", { case t: StringToken => t.value })
  }

  def integer: Parser[Long] = {
    accept("integer", { case t: IntToken => t.value })
  }

  def localSymbol: Parser[String] = {
    accept("local symbol", { case t: SymbolToken if !t.isGlobal => t.value })
  }

  def globalSymbol: Parser[String] = {
    accept("global symbol", { case t: SymbolToken if t.isGlobal => t.value })
  }

  def label: Parser[String] = {
    accept("label", { case t: LabelToken => t.value })
  }

  def test(input: String) = {
    val reader = new Lexer.Scanner(input)
    val result = phrase(module)(reader)
    result match {
      case Success(ast, _) => ast
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}
