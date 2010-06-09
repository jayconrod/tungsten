package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader
import java.io.File

object Parser extends Parsers with ImplicitConversions {
  type Elem = Lexer.Token

  private val symbolFactory = new SymbolFactory

  def module(file: File): Parser[(Module, List[AstNode])] = {
    module ^^ { 
      case (m, ds) => (m.copyWith(filename=Some(file)), ds)
    }
  }

  lazy val module: Parser[(Module, List[AstNode])] = {
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
    annotation | function | global | struct
  }

  lazy val annotation: Parser[AstNode] = {
    annotations ~ ("annotation" ~> symbol) ~ children(parameter, "(", ",", ")") ^^ {
      case anns ~ n ~ ps => {
        val annotation = Annotation(n, childNames(ps, n), anns)
        AstNode(annotation, ps)
      }
    }
  }

  lazy val function: Parser[AstNode] = {
    annotations ~ ("function" ~> ty) ~ symbol ~ children(parameter, "(", ",", ")") ~
      children(block, "{", "", "}") ^^ {
        case anns ~ rty ~ n ~ ps ~ bs => {
          val function = Function(n, rty, childNames(ps, n), childNames(bs, n), anns)
          AstNode(function, ps ++ bs)
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
        val struct = Struct(n, childNames(fs, n), anns)
        AstNode(struct, fs)
      }
    }
  }

  lazy val block: Parser[AstNode] = {
    annotations ~ ("block" ~> symbol) ~ children(parameter, "(", ",", ")") ~ 
      children(instructionDefn, "{", "", "}") ^^ {
        case anns ~ n ~ ps ~ is => {
          val block = Block(n, childNames(ps, n), childNames(is, n), anns)
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

  def childNames(children: List[AstNode], parentName: Symbol): List[Symbol] = {
    children map { child =>
      val fullChildName = child.name.name
      val prefix = fullChildName.head.charAt(0)
      val isGlobal = prefix == '@'
      if (isGlobal)
        child.name
      else {
        val strippedChildName = fullChildName.head.substring(1) :: fullChildName.tail
        val newChildName = parentName.name ++ strippedChildName
        Symbol(newChildName, child.name.id)
      }
    }
  }

  lazy val instructionDefn: Parser[AstNode] = {
    instruction ^^ { i => AstNode(i, Nil) }
  }

  lazy val instruction: Parser[Instruction] = {
    addressInst        |
    assignInst         |
    binopInst          |
    branchInst         |
    condInst           |
    fextendInst        |
    ftoiInst           |
    ftruncateInst      |
    heapInst           |
    heapArrayInst      |
    itofInst           |
    isextendInst       |
    itruncateInst      |
    izextendInst       |
    intrinsicInst      |
    loadInst           |
    loadElementInst    |
    relopInst          |
    returnInst         |
    storeInst          |
    storeElementInst   |
    stackInst          |
    stackArrayInst     |
    scallInst          |
    upcastInst
  }

  lazy val addressInst: Parser[AddressInstruction] = {
    instName("address") ~ (value <~ ",") ~ rep1sep(value, ",") ^^ {
      case anns ~ ty ~ n ~ a ~ is => AddressInstruction(n, ty, a, is, anns)
    }
  }

  lazy val assignInst: Parser[AssignInstruction] = {
    instName("assign") ~ value ^^ {
      case anns ~ ty ~ n ~ v => AssignInstruction(n, ty, v, anns)
    }
  }

  lazy val binopInst: Parser[BinaryOperatorInstruction] = {
    instName("binop") ~ value ~ binop ~ value ^^ {
      case anns ~ ty ~ n ~ l ~ op ~ r => BinaryOperatorInstruction(n, ty, op, l, r, anns)
    }
  }

  lazy val branchInst: Parser[BranchInstruction] = {
    instName("branch") ~ symbol ~ argumentList ^^ {
      case anns ~ ty ~ n ~ b ~ as => BranchInstruction(n, ty, b, as, anns)
    }
  }

  lazy val condInst: Parser[ConditionalBranchInstruction] = {
    instName("cond") ~ (value <~ "?") ~ symbol ~ (argumentList <~ ":") ~ symbol ~ argumentList ^^ {
      case anns ~ ty ~ n ~ c ~ tb ~ tas ~ fb ~ fas => {
        ConditionalBranchInstruction(n, ty, c, tb, tas, fb, fas, anns)
      }
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
    instName("heap", false) ^^ {
      case anns ~ ty ~ n => HeapAllocateInstruction(n, ty, anns)
    }
  }

  lazy val heapArrayInst: Parser[HeapAllocateArrayInstruction] = {
    instName("heaparray") ~ (value <~ "x") ~ ty ^^ {
      case anns ~ ty ~ n ~ v ~ t => HeapAllocateArrayInstruction(n, ty, v, t, anns)
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
    instName("stack", false) ^^ {
      case anns ~ ty ~ n => StackAllocateInstruction(n, ty, anns)
    }
  }

  lazy val stackArrayInst: Parser[StackAllocateArrayInstruction] = {
    instName("stackarray") ~ (value <~ "x") ~ ty ^^ {
      case anns ~ ty ~ n ~ v ~ t => StackAllocateArrayInstruction(n, ty, v, t, anns)
    }
  }

  lazy val scallInst: Parser[StaticCallInstruction] = {
    instName("scall") ~ symbol ~ argumentList ^^ {
      case anns ~ ty ~ n ~ f ~ as => StaticCallInstruction(n, ty, f, as, anns)
    }
  }

  lazy val upcastInst: Parser[UpcastInstruction] = {
    instName("upcast") ~ value ^^ {
      case anns ~ ty ~ n ~ v => UpcastInstruction(n, ty, v, anns)
    }
  }

  lazy val annotations: Parser[List[AnnotationValue]] = rep(annotationValue)

  lazy val annotationValue: Parser[AnnotationValue] = {
    symbol ~ opt(argumentList) ^^ {
      case name ~ None => AnnotationValue(name, Nil)
      case name ~ Some(args) => AnnotationValue(name, args)
    }
  }

  def instName(name: String, 
               useEquals: Boolean = true): Parser[List[AnnotationValue] ~ Type ~ Symbol] = 
  {
    val optName: Parser[Type ~ Symbol] = {
      val nameParser = if (useEquals)
        ty ~ symbol <~ "="
      else
        ty ~ symbol
      opt(nameParser) ^^ { _.getOrElse(new ~(UnitType, symbolFactory.symbol("%anon$"))) }
    }
    annotations ~ (name ~> optName) ^^ {
      case anns ~ (ty ~ n) => new ~(new ~(anns, ty), n)
    }
  }

  lazy val argumentList: Parser[List[Value]] = {
    "(" ~> repsep(value, ",") <~ ")"
  }

  def children(parser: Parser[AstNode], 
               prefix: String, 
               separator: String, 
               suffix: String): Parser[List[AstNode]] =
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

    ("(" ~ ")"           ^^^ UnitValue)                  |
    ("true"              ^^^ BooleanValue(true))         |
    ("false"             ^^^ BooleanValue(false))        |
    (char                 ^^ { v => CharValue(v) })      |
    (string               ^^ { v => StringValue(v) })    |
    ("int8" ~> integer    ^^ { v => IntValue(v, 8) })    |
    ("int16" ~> integer   ^^ { v => IntValue(v, 16) })   |
    ("int32" ~> integer   ^^ { v => IntValue(v, 32) })   |
    ("int64" ~> integer   ^^ { v => IntValue(v, 64) })   |
    ("float32" ~> float   ^^ { v => FloatValue(v, 32) }) |
    ("float64" ~> float   ^^ { v => FloatValue(v, 64) }) |
    ("null"              ^^^ NullValue)                  |
    arrayValue                                           |
    structValue                                          |
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
      structTy                       |
      arrayTy 
    }
                  
    basicTy ~ rep("*") ^^ { case ety ~ stars => makePointerType(ety, stars.size) }    
  }

  lazy val arrayTy: Parser[ArrayType] = {
    "[" ~> (integer <~ "x") ~ ty <~ "]" ^^ { case s ~ ety => ArrayType(s, ety) }
  }

  lazy val structTy: Parser[StructType] = {
    "struct" ~> symbol ^^ { case name => StructType(name) }
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
    ("exit" ^^^ EXIT)
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
}
