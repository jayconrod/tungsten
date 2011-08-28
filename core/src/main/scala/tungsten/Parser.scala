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
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader
import java.io.File
import Utilities._

class Parser extends Parsers with ImplicitConversions {
  type Elem = Lexer.Token

  private val symbolFactory = new SymbolFactory

  def module(file: File): Parser[(Module, List[AstNode])] = {
    module ^^ { case (m, ds) => (m.copyWith(filename=Some(file)), ds) }
  }

  def module: Parser[(Module, List[AstNode])] = {
    unglobalizedModule ^^ { case (m, ds) => (m, ds.map(_.globalize(None)))}
  }

  lazy val unglobalizedModule: Parser[(Module, List[AstNode])] = {
    headers ~ rep(definition) ^^ {
      case m ~ ds => (m, ds)
    }
  }

  lazy val headers: Parser[Module] = {
    opt("name" ~> ":" ~> symbol) ~
    opt("type" ~> ":" ~> ("intermediate" | "library" | "program")) ~
    opt("version" ~> ":" ~> version) ~
    opt("filename" ~> ":" ~> string) ~
    opt("dependencies" ~> ":" ~> repsep(moduleDependency, ",")) ~
    opt("searchpaths" ~> ":" ~> repsep(string, ",")) ~
    opt("is64bit" ~> ":" ~> boolean) ~
    opt("safe" ~> ":" ~> boolean) ^^ {
      case n ~ t ~ v ~ f ~ ds ~ sps ~ b ~ s => {
        val defaultValues = new Module
        def stripPrefix(sym: Symbol) = {
          val name = sym.name
          Symbol(name.head.substring(1) :: name.tail, sym.id)
        }
        val name = n.map(stripPrefix _).getOrElse(defaultValues.name)
        val ty = t match {
          case Some("intermediate") => ModuleType.INTERMEDIATE
          case Some("library") => ModuleType.LIBRARY
          case Some("program") => ModuleType.PROGRAM
          case None => defaultValues.ty
          case _ => throw new AssertionError("Invalid module type")
        }
        val version = v.getOrElse(defaultValues.version)
        val filename = f.map(new File(_))
        val dependencies = ds.getOrElse(defaultValues.dependencies)
        val searchPaths = sps.map(_.map(new File(_))).getOrElse(defaultValues.searchPaths)
        val is64Bit = b.getOrElse(defaultValues.is64Bit)
        val isSafe = b.getOrElse(defaultValues.isSafe)
        new Module(name=name,
                   ty=ty,
                   version=version,
                   filename=filename,
                   dependencies=dependencies,
                   searchPaths=searchPaths,
                   is64Bit=is64Bit,
                   isSafe=isSafe)
      }
    }
  }

  lazy val definition: Parser[AstNode] = {
    annotation | clas | interface | function | global | struct
  }

  lazy val annotation: Parser[AstNode] = {
    annotations ~ ("annotation" ~> symbol) ~ children(parameter, "(", ",", ")") ^^ {
      case anns ~ n ~ ps => {
        val annotation = Annotation(n, ps.map(_.name), anns)
        AstNode(annotation, ps)
      }
    }
  }

  lazy val clas: Parser[AstNode] = {
    val tyParams: Parser[List[AstNode]] = children(typeParameter, "[", ",", "]")
    val superclass: Parser[Option[ClassType]] = opt("<:" ~> classTy)
    val inheritedInterface: Parser[(InterfaceType, List[Symbol])] = {
      interfaceTy ~ children(symbol, "{", ",", "}") ^^ { case t ~ ms => (t, ms) }
    }
    val fields: Parser[List[AstNode]] = rep(field)
    val body = opt("{" ~> rep(inheritedInterface) ~ constructors ~ methods ~ fields <~ "}") ^^ {
      case Some(b) => b
      case None => new ~(new ~(new ~(Nil, Nil), Nil), Nil)
    }
    annotations ~ ("class" ~> symbol) ~ tyParams ~ superclass ~ body ^^ {
        case anns ~ n ~ tps ~ sc ~ (is ~ cs ~ ms ~ fs) => {
          val (its, ims) = is.unzip
          val clas = Class(n, tps.map(_.name), sc, its, ims, cs, ms, fs.map(_.name), anns)
          AstNode(clas, tps ++ fs)
        }
    }
  }

  lazy val interface: Parser[AstNode] = {
    val inheritedInterface: Parser[(InterfaceType, List[Symbol])] = {
      interfaceTy ~ children(symbol, "{", ",", "}") ^^ { case t ~ ms => (t, ms) }
    }
    val body: Parser[~[List[(InterfaceType, List[Symbol])], List[Symbol]]] = {
      opt("{" ~> rep(inheritedInterface) ~ methods <~ "}") ^^ {
        case Some(is ~ ms) => new ~(is, ms)
        case None => new ~(Nil, Nil)
      }
    }
    annotations ~ ("interface" ~> symbol) ~ 
      children(typeParameter, "[", ",", "]") ~ ("<:" ~> (classTy | interfaceTy)) ~ body ^^ 
    {
      case anns ~ n ~ tps ~ sc ~ (is ~ ms) => {
        val (its, ims) = is.unzip
        val interface = Interface(n, tps.map(_.name), sc, its, ims, ms, anns)
        AstNode(interface, tps)
      }
    }
  }


  lazy val function: Parser[AstNode] = {
    annotations ~ ("function" ~> ty) ~ symbol ~ 
      children(typeParameter, "[", ",", "]") ~
      children(parameter, "(", ",", ")") ~
      children(block, "{", "", "}") ^^ {
        case anns ~ rty ~ n ~ tps ~ ps ~ bs => {
          val function = Function(n, rty, tps.map(_.name), ps.map(_.name), bs.map(_.name), anns)
          AstNode(function, tps ++ ps ++ bs)
        }
    }
  }

  lazy val global: Parser[AstNode] = {
    annotations ~ ("global" ~> ty) ~ symbol ~ opt("=" ~> value) ^^ {
      case anns ~ t ~ n ~ v => {
        val global = Global(n, t, v, anns)
        AstNode(global, Nil)
      }
    }
  }

  lazy val struct: Parser[AstNode] = {
    annotations ~ ("struct" ~> symbol) ~ children(field, "{", "", "}") ^^ {
      case anns ~ n ~ fs => {
        val struct = Struct(n, fs.map(_.name), anns)
        AstNode(struct, fs)
      }
    }
  }

  lazy val block: Parser[AstNode] = {
    annotations ~ ("block" ~> symbol) ~ children(parameter, "(", ",", ")") ~ 
      children(instructionDefn, "{", "", "}") ~
      opt("catch" ~> symbol ~ argumentList) ^^ {
        case anns ~ n ~ ps ~ is ~ c => {
          val cb = c map { case b ~ as => (b, as) }
          val block = Block(n, ps.map(_.name), is.map(_.name), cb, anns)
          AstNode(block, ps ++ is)
        }
    }
  }

  lazy val field: Parser[AstNode] = {
    annotations ~ ("field" ~> ty) ~ symbol ^^ {
      case anns ~ t ~ n => { 
        val field = Field(n, t, anns)
        AstNode(field, Nil)
      }
    }
  }

  lazy val parameter: Parser[AstNode] = {
    annotations ~ ty ~ symbol ^^ {
      case anns ~ t ~ n => {
        val parameter = Parameter(n, t, anns)
        AstNode(parameter, Nil)
      }
    }
  }

  lazy val typeParameter: Parser[AstNode] = {
    val variance: Parser[Variance] = {
      import Variance._
      opt("+" | "-") ^^ {
        case Some("+") => COVARIANT
        case Some("-") => CONTRAVARIANT
        case None => INVARIANT
      }
    }
    annotations ~ ("type" ~> variance) ~ symbol ~ opt("<:" ~> ty) ~ opt(">:" ~> ty) ^^ {
      case anns ~ v ~ n ~ u ~ l => {
        val tyParam = TypeParameter(n, u, l, v, anns)
        AstNode(tyParam, Nil)
      }
    }
  }

  lazy val constructors: Parser[List[Symbol]] = {
    opt("constructors" ~> children(symbol, "{", ",", "}")) ^^ {
      case Some(cs) => cs
      case None => Nil
    }
  }

  lazy val methods: Parser[List[Symbol]] = {
    opt("methods" ~> children(symbol, "{", ",", "}")) ^^ {
      case Some(ms) => ms
      case None => Nil
    }
  }

  lazy val instructionDefn: Parser[AstNode] = {
    instruction ^^ { i => AstNode(i, Nil) }
  }

  lazy val instruction: Parser[Instruction] = {
    addressInst        |
    binopInst          |
    bitcastInst        |
    branchInst         |
    catchInst          |
    condInst           |
    extractInst        |
    fextendInst        |
    ftoiInst           |
    ftruncateInst      |
    heapInst           |
    heapArrayInst      |
    insertInst         |
    itofInst           |
    isextendInst       |
    itruncateInst      |
    izextendInst       |
    intrinsicInst      |
    loadInst           |
    loadElementInst    |
    newInst            |
    pcallInst          |
    relopInst          |
    returnInst         |
    storeInst          |
    storeElementInst   |
    stackInst          |
    stackArrayInst     |
    scallInst          |
    unreachableInst    |
    upcastInst         |
    throwInst          |
    vcallInst          |
    vlookupInst
  }

  lazy val addressInst: Parser[AddressInstruction] = {
    instName("address") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case anns ~ ty ~ n ~ a ~ is => AddressInstruction(n, ty, a, is, anns)
    }
  }

  lazy val binopInst: Parser[BinaryOperatorInstruction] = {
    instName("binop") ~ value ~ binop ~ value ^^ {
      case anns ~ ty ~ n ~ l ~ op ~ r => BinaryOperatorInstruction(n, ty, op, l, r, anns)
    }
  }

  lazy val bitcastInst: Parser[BitCastInstruction] = {
    instName("bitcast") ~ value ^^ {
      case anns ~ ty ~ n ~ v => BitCastInstruction(n, ty, v, anns)
    }
  }

  lazy val branchInst: Parser[BranchInstruction] = {
    instName("branch") ~ symbol ~ argumentList ^^ {
      case anns ~ ty ~ n ~ b ~ as => BranchInstruction(n, ty, b, as, anns)
    }
  }

  lazy val catchInst: Parser[CatchInstruction] = {
    instName("catch") ^^ {
      case anns ~ ty ~ n => CatchInstruction(n, ty, anns)
    }
  }

  lazy val condInst: Parser[ConditionalBranchInstruction] = {
    instName("cond") ~ (value <~ "?") ~ symbol ~ (argumentList <~ ":") ~ symbol ~ argumentList ^^ {
      case anns ~ ty ~ n ~ c ~ tb ~ tas ~ fb ~ fas => {
        ConditionalBranchInstruction(n, ty, c, tb, tas, fb, fas, anns)
      }
    }
  }

  lazy val extractInst: Parser[ExtractInstruction] = {
    instName("extract") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case anns ~ ty ~ n ~ v ~ is => ExtractInstruction(n, ty, v, is, anns)
    }
  }

  lazy val fextendInst: Parser[FloatExtendInstruction] = {
    instName("fextend") ~ value ^^ {
      case anns ~ ty ~ n ~ v => FloatExtendInstruction(n, ty, v, anns)
    }
  }

  lazy val ftoiInst: Parser[FloatToIntegerInstruction] = {
    instName("ftoi") ~ value ^^ {
      case anns ~ ty ~ n ~ v => FloatToIntegerInstruction(n, ty, v, anns)
    }
  }

  lazy val ftruncateInst: Parser[FloatTruncateInstruction] = {
    instName("ftruncate") ~ value ^^ {
      case anns ~ ty ~ n ~ v => FloatTruncateInstruction(n, ty, v, anns)
    }
  }

  lazy val heapInst: Parser[HeapAllocateInstruction] = {
    instName("heap") ^^ {
      case anns ~ ty ~ n => HeapAllocateInstruction(n, ty, anns)
    }
  }

  lazy val heapArrayInst: Parser[HeapAllocateArrayInstruction] = {
    instName("heaparray") ~ value ^^ {
      case anns ~ ty ~ n ~ v => HeapAllocateArrayInstruction(n, ty, v, anns)
    }
  }

  lazy val insertInst: Parser[InsertInstruction] = {
    instName("insert") ~ (value <~ ",") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case anns ~ ty ~ n ~ v ~ b ~ is => InsertInstruction(n, ty, v, b, is, anns)
    }
  }

  lazy val itofInst: Parser[IntegerToFloatInstruction] = {
    instName("itof") ~ value ^^ {
      case anns ~ ty ~ n ~ v => IntegerToFloatInstruction(n, ty, v, anns)
    }
  }

  lazy val isextendInst: Parser[IntegerSignExtendInstruction] = {
    instName("isextend") ~ value ^^ {
      case anns ~ ty ~ n ~ v => IntegerSignExtendInstruction(n, ty, v, anns)
    }
  }

  lazy val itruncateInst: Parser[IntegerTruncateInstruction] = {
    instName("itruncate") ~ value ^^ {
      case anns ~ ty ~ n ~ v => IntegerTruncateInstruction(n, ty, v, anns)
    }
  }

  lazy val izextendInst: Parser[IntegerZeroExtendInstruction] = {
    instName("izextend") ~ value ^^ {
      case anns ~ ty ~ n ~ v => IntegerZeroExtendInstruction(n, ty, v, anns)
    }
  }

  lazy val intrinsicInst: Parser[IntrinsicCallInstruction] = {
    instName("intrinsic") ~ intrinsic ~ argumentList ^^ {
      case anns ~ ty ~ n ~ i ~ as => IntrinsicCallInstruction(n, ty, i, as, anns)
    }
  }

  lazy val loadInst: Parser[LoadInstruction] = {
    instName("load") ~ value ^^ {
      case anns ~ ty ~ n ~ v => LoadInstruction(n, ty, v, anns)
    }
  }

  lazy val loadElementInst: Parser[LoadElementInstruction] = {
    instName("loadelement") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case anns ~ ty ~ n ~ v ~ is => LoadElementInstruction(n, ty, v, is, anns)
    }
  }

  lazy val newInst: Parser[NewInstruction] = {
    instName("new") ~ symbol ~ typeArgumentList ~ argumentList ^^ {
      case anns ~ ty ~ n ~ c ~ tas ~ as => NewInstruction(n, ty, c, tas, as, anns)
    }
  }

  lazy val pcallInst: Parser[PointerCallInstruction] = {
    instName("pcall") ~ value ~ typeArgumentList ~ argumentList ^^ {
      case anns ~ ty ~ n ~ t ~ tas ~ as => PointerCallInstruction(n, ty, t, tas, as, anns)
    }
  }

  lazy val relopInst: Parser[RelationalOperatorInstruction] = {
    instName("relop") ~ value ~ relop ~ value ^^ {
      case anns ~ ty ~ n ~ l ~ op ~ r => RelationalOperatorInstruction(n, ty, op, l, r, anns)
    }
  }

  lazy val returnInst: Parser[ReturnInstruction] = {
    instName("return") ~ value ^^ {
      case anns ~ ty ~ n ~ v => ReturnInstruction(n, ty, v, anns)
    }
  }

  lazy val storeInst: Parser[StoreInstruction] = {
    instName("store") ~ (value <~ ",") ~ value ^^ {
      case anns ~ ty ~ n ~ v ~ p => StoreInstruction(n, ty, v, p, anns)
    }
  }

  lazy val storeElementInst: Parser[StoreElementInstruction] = {
    instName("storeelement") ~ (value <~ ",") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case anns ~ ty ~ n ~ v ~ p ~ is => StoreElementInstruction(n, ty, v, p, is, anns)
    }
  }

  lazy val stackInst: Parser[StackAllocateInstruction] = {
    instName("stack") ^^ {
      case anns ~ ty ~ n => StackAllocateInstruction(n, ty, anns)
    }
  }

  lazy val stackArrayInst: Parser[StackAllocateArrayInstruction] = {
    instName("stackarray") ~ value ^^ {
      case anns ~ ty ~ n ~ v => StackAllocateArrayInstruction(n, ty, v, anns)
    }
  }

  lazy val scallInst: Parser[StaticCallInstruction] = {
    instName("scall") ~ symbol ~ typeArgumentList ~ argumentList ^^ {
      case anns ~ ty ~ n ~ f ~ tas ~ as => StaticCallInstruction(n, ty, f, tas, as, anns)
    }
  }

  lazy val throwInst: Parser[ThrowInstruction] = {
    instName("throw") ~ value ^^ {
      case anns ~ ty ~ n ~ v => ThrowInstruction(n, ty, v, anns)
    }
  }

  lazy val unreachableInst: Parser[UnreachableInstruction] = {
    instName("unreachable") ^^ {
      case anns ~ ty ~ n => UnreachableInstruction(n, ty, anns)
    }
  }

  lazy val upcastInst: Parser[UpcastInstruction] = {
    instName("upcast") ~ value ^^ {
      case anns ~ ty ~ n ~ v => UpcastInstruction(n, ty, v, anns)
    }
  }

  lazy val vcallInst: Parser[VirtualCallInstruction] = {
    instName("vcall") ~ value ~ (":" ~> integer) ~ typeArgumentList ~ argumentList ^^ {
      case anns ~ ty ~ n ~ o ~ i ~ tas ~ as => 
        VirtualCallInstruction(n, ty, o, i.toInt, tas, as, anns)
    }
  }

  lazy val vlookupInst: Parser[VirtualLookupInstruction] = {
    instName("vlookup") ~ value ~ (":" ~> integer) ^^ {
      case anns ~ ty ~ n ~ o ~ i =>
        VirtualLookupInstruction(n, ty, o, i.toInt, anns)
    }
  }

  lazy val annotations: Parser[List[AnnotationValue]] = rep(annotationValue)

  lazy val annotationValue: Parser[AnnotationValue] = {
    symbol ~ opt(argumentList) ^^ {
      case name ~ None => AnnotationValue(name, Nil)
      case name ~ Some(args) => AnnotationValue(name, args)
    }
  }

  def instName(opName: String): Parser[List[AnnotationValue] ~ Type ~ Symbol] = {
    annotations ~ opt(ty ~ symbol <~ "=") <~ opName ^^ {
      case anns ~ Some(t ~ n) => new ~(new ~(anns, t), n)
      case anns ~ None => new ~(new ~(anns, UnitType), symbolFactory.symbol("%anon$"))
    }
  }

  lazy val argumentList: Parser[List[Value]] = {
    "(" ~> repsep(value, ",") <~ ")"
  }

  def children[T](parser: Parser[T], 
                  prefix: String, 
                  separator: String, 
                  suffix: String): Parser[List[T]] =
  {
    val listParser = if (separator.isEmpty)
      rep(parser)
    else
      repsep(parser, separator)
    opt(prefix ~> listParser <~ suffix) ^^ {
      case Some(cs) => cs
      case None => Nil
    }
  }

  lazy val value: Parser[Value] = {
    def arrayValue: Parser[Value] = {
      arrayTy ~ ("{" ~> repsep(value, ",") <~ "}") ^? { 
        case ArrayType(length, elementType) ~ es if length == es.size => {
          ArrayValue(elementType, es)
        }
      }
    }
    def structValue: Parser[Value] = {
      structTy ~ ("{" ~> repsep(value, ",") <~ "}") ^? {
        case StructType(name) ~ es => StructValue(name, es)
      }
    }
    def bitCastValue: Parser[Value] = {
      "bitcast" ~> value ~ ("to" ~> ty) ^^ {
        case v ~ t => BitCastValue(v, t)
      }
    }
    def stringValue: Parser[Value] = {
      string ^^ { s =>
        ArrayValue(IntType(8),
                   s.getBytes("UTF-8").toList.map { c => IntValue(c, 8) })
      }
    }

    ("(" ~ ")"           ^^^ UnitValue)                  |
    ("true"              ^^^ BooleanValue(true))         |
    ("false"             ^^^ BooleanValue(false))        |
    (char                 ^^ { v => CharValue(v) })      |
    ("int8" ~> integer    ^^ { v => IntValue(v, 8) })    |
    ("int16" ~> integer   ^^ { v => IntValue(v, 16) })   |
    ("int32" ~> integer   ^^ { v => IntValue(v, 32) })   |
    ("int64" ~> integer   ^^ { v => IntValue(v, 64) })   |
    ("float32" ~> float   ^^ { v => FloatValue(v, 32) }) |
    ("float64" ~> float   ^^ { v => FloatValue(v, 64) }) |
    ("null"              ^^^ NullValue)                  |
    arrayValue                                           |
    stringValue                                          |
    structValue                                          |
    bitCastValue                                         |
    (ty ~ symbol          ^^ { case t ~ n => DefinedValue(n, t) })
  }

  lazy val ty: Parser[Type] = {
    def makePointerType(elementType: Type, count: Int): Type = {
      if (count == 0)
        elementType
      else
        makePointerType(PointerType(elementType), count - 1)
    }
    def basicTy: Parser[Type] = {
      functionTy                     |
      ("unit"     ^^^ UnitType)      |
      ("boolean"  ^^^ BooleanType)   |
      ("char"     ^^^ CharType)      |
      ("string"   ^^^ StringType)    |
      ("int8"     ^^^ IntType(8))    |
      ("int16"    ^^^ IntType(16))   |
      ("int32"    ^^^ IntType(32))   |
      ("int64"    ^^^ IntType(64))   |
      ("float32"  ^^^ FloatType(32)) |
      ("float64"  ^^^ FloatType(64)) |
      ("nulltype" ^^^ NullType)      |
      ("..."      ^^^ VariadicType)  |
      structTy                       |
      arrayTy                        |
      classTy                        |
      interfaceTy                    |
      variableTy
    }
                  
    basicTy ~ rep("*") ^^ { case ety ~ stars => makePointerType(ety, stars.size) }    
  }

  lazy val structTy: Parser[StructType] = {
    "struct" ~> symbol ^^ { case name => StructType(name) }
  }

  lazy val functionTy: Parser[FunctionType] = {
    children(symbol, "[", ",", "]") ~ children(ty, "(", ",", ")") ~ ("->" ~> ty) ^^ {
      case tps ~ pts ~ rt => FunctionType(rt, tps, pts)
    }
  }

  lazy val arrayTy: Parser[ArrayType] = {
    "[" ~> (integer <~ "x") ~ ty <~ "]" ^^ { case s ~ ety => ArrayType(s, ety) }
  }

  lazy val classTy: Parser[ClassType] = {
    "class" ~> symbol ~ typeArgumentList ^^ {
      case n ~ as => ClassType(n, as)
    }
  }

  lazy val interfaceTy: Parser[InterfaceType] = {
    "interface" ~> symbol ~ typeArgumentList ^^ {
      case n ~ as => InterfaceType(n, as)
    }
  }

  lazy val variableTy: Parser[VariableType] = {
    "type" ~> symbol ^^ { case n => VariableType(n) }
  }

  lazy val typeArgumentList: Parser[List[Type]] = {
    opt("[" ~> rep1sep(ty, ",") <~ "]") ^^ {
      case Some(as) => as
      case None => Nil
    }
  }

  implicit def reserved(r: String): Parser[String] = {
    assert(Lexer.reservedStrings(r))
    elem(ReservedTok(r)) ^^^ r
  }
  lazy val symbol: Parser[Symbol] = accept("symbol", { case SymbolTok(v) => v })
  lazy val integer: Parser[Long] = accept("integer", { case IntTok(v) => v })
  lazy val boolean: Parser[Boolean] = {
    ("true" ^^^ true) |
    ("false" ^^^ false)
  }
  lazy val float: Parser[Double] = accept("float", { case FloatTok(v) => v })
  lazy val char: Parser[Char] = accept("char", { case CharTok(v) => v })
  lazy val string: Parser[String] = accept("string", { case StringTok(v) => v })
  lazy val binop: Parser[BinaryOperator] = {
    import BinaryOperator._
    ("*"   ^^^ MULTIPLY)               |
    ("/"   ^^^ DIVIDE)                 |
    ("%"   ^^^ REMAINDER)              |
    ("+"   ^^^ ADD)                    |
    ("-"   ^^^ SUBTRACT)               |
    ("<<"  ^^^ LEFT_SHIFT)             |
    (">>"  ^^^ RIGHT_SHIFT_ARITHMETIC) |
    (">>>" ^^^ RIGHT_SHIFT_LOGICAL)    |
    ("&"   ^^^ AND)                    |
    ("^"   ^^^ XOR)                    |
    ("|"   ^^^ OR)
  }
  lazy val relop: Parser[RelationalOperator] = {
    import RelationalOperator._
    ("<"  ^^^ LESS_THAN)     |
    ("<=" ^^^ LESS_EQUAL)    |
    (">"  ^^^ GREATER_THAN)  |
    (">=" ^^^ GREATER_EQUAL) |
    ("==" ^^^ EQUAL)         |
    ("!=" ^^^ NOT_EQUAL)
  }
  lazy val intrinsic: Parser[IntrinsicFunction] = {
    import Intrinsic._
    ("exit"         ^^^ EXIT)           |
    ("read"         ^^^ READ)           |
    ("write"        ^^^ WRITE)          |
    ("open"         ^^^ OPEN)           |
    ("close"        ^^^ CLOSE)
  }
  lazy val version: Parser[Version] = accept("version", { case VersionTok(v) => v })
  lazy val moduleDependency: Parser[ModuleDependency] = {
    accept("module dependency", { case ModuleDependencyTok(v) => v })
  }

  def test[T](input: String, parser: Parser[T]): T = {
    val reader = new Lexer.Scanner(input)
    phrase(parser)(reader) match {
      case Success(r, _) => r
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}

final case class AstNode(definition: Definition, children: List[AstNode]) {
  def name = definition.name

  def toList: List[Definition] = {
    definition :: children.map(_.toList).flatten
  }

  def globalize (parent: Option[Symbol]): AstNode = {
    val globalizedDefn = globalizeDefn(parent)
    AstNode(globalizedDefn, children.map(_.globalize(Some(globalizedDefn.name))))
  }

  def globalizeDefn(parent: Option[Symbol]): Definition = {
    val globalizedName = globalizeSymbol(definition.name, parent)
    val newParent = if (definitionCanHaveChildren) Some(globalizedName) else parent
    definition.copyWith(("name" -> globalizedName)).
      mapSymbols(globalizeSymbol(_, newParent))
  }

  /** Returns whether the definition is cabable of having child definitions, based on its
   *  class. Definitions which cannot have children, such as parameters or instructions, are
   *  allowed to refer to siblings (definitions at the same level) directly with the % prefix.
   *  Definitions which cannot have children cannot do this.
   */
  def definitionCanHaveChildren: Boolean = {
    definition.isInstanceOf[Annotation]  ||
      definition.isInstanceOf[Block]     ||
      definition.isInstanceOf[Function]  ||
      definition.isInstanceOf[Interface] ||
      definition.isInstanceOf[Class]     ||
      definition.isInstanceOf[Struct]
  }

  def globalizeSymbol(symbol: Symbol, parent: Option[Symbol]): Symbol = {
    val prefix = symbol.name.head(0)
    val unprefixedName = symbol.name.head.tail :: symbol.name.tail
    prefix match {
      case '@' => Symbol(unprefixedName, symbol.id)
      case '%' => {
        parent match {
          case None => Symbol(unprefixedName, symbol.id)
          case Some(parentSymbol) => {
            val parentName = parentSymbol.name
            Symbol(parentName ++ unprefixedName, symbol.id)
          }
        }
      }
      case _ => symbol
    }
  }
}
