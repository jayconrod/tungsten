package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader
import java.io.File

object AstParser extends Parsers with ImplicitConversions {
  type Elem = Token

  private val symbolFactory = new SymbolFactory

  def symbol: Parser[Symbol] = {
    elem("symbol", _.isInstanceOf[SymbolToken]) ^^ { 
      case SymbolToken(sym) => sym
      case _ => throw new AssertionError
    }
  }

  def version: Parser[Version] = {
    elem("version", _.isInstanceOf[VersionToken]) ^^ { _.asInstanceOf[VersionToken].version }
  }

  def dependency: Parser[ModuleDependency] = {
    elem("module dependency", _.isInstanceOf[ModuleDependencyToken]) ^^ {
      _.asInstanceOf[ModuleDependencyToken].dependency
    } |
    symbol ^^ { ModuleDependency(_, Version.MIN, Version.MAX) }
  }

  def string: Parser[String] = {
    elem("string", _.isInstanceOf[StringToken]) ^^ {
      _.asInstanceOf[StringToken].value
    }
  }

  def ty: Parser[AstType] = {
    def arrayTy: Parser[AstType] = {
      def size: Parser[Option[Int]] = {
        accept("int", { case IntToken(i) => Some(i) }) | ("?" ^^^ None)
      }
      "[" ~> (size <~ "*") ~ (ty <~ "]") ^^ { 
        case s ~ t => AstArrayType(s, t)
      }
    }
    def elementTy: Parser[AstType] = {
      ("#unit" ^^^ AstUnitType) |
      ("#boolean" ^^^ AstBooleanType) |
      ("#int8" ^^^ AstIntType(8)) |
      ("#int16" ^^^ AstIntType(16)) |
      ("#int32" ^^^ AstIntType(32)) |
      ("#int64" ^^^ AstIntType(64)) |
      ("#float32" ^^^ AstFloatType(32)) |
      ("#float64" ^^^ AstFloatType(64)) |
      ("#null" ^^^ AstNullType) |
      arrayTy |
      (symbol ~ typeArguments ^^ { 
        case name ~ args => AstClassType(name, args)
      })
    }
    def makePointerType(ety: AstType, stars: Int): AstType = {
      if (stars == 0)
        ety
      else {
        val pty = AstPointerType(ety)
        makePointerType(pty, stars - 1)
      }
    }

    elementTy ~ rep("*") ^^ { 
      case ety ~ stars => makePointerType(ety, stars.size)
    }   
  }

  def typeArguments: Parser[List[AstType]] = {
    opt("[" ~> rep1sep(ty, ",") <~ "]") ^^ {
      case Some(types) => types
      case None => Nil
    }
  }

  def boolean: Parser[Boolean] = {
    ("#true" ^^^ true) | ("#false" ^^^ false)
  }

  def value: Parser[AstValue] = {
    def byte = accept("byte", { case ByteToken(b) => b })
    def short = accept("short", { case ShortToken(s) => s })
    def int = accept("int", { case IntToken(i) => i })
    def long = accept("long", { case LongToken(l) => l })
    def float = accept("float", { case Float32Token(f) => f })
    def double = accept("double", { case Float64Token(d) => d })

    def array = {
      "[" ~> (ty <~ ":") ~ (repsep(value, ",") <~"]") ^^ 
        flatten2(AstArrayValue(_, _))
    }

    def aggregate = {
      "{" ~> (symbol <~ ":") ~ (repsep(value, ",") <~ "}") ^^
        flatten2(AstAggregateValue(_, _))
    }

    ("(" ~ ")" ^^^ AstUnitValue) |
    (boolean ^^ { case b => AstBooleanValue(b) }) |
    (byte ^^ { AstInt8Value(_) }) |
    (short ^^  { AstInt16Value(_) }) |
    (int ^^ { AstInt32Value(_) }) |
    (long ^^ { AstInt64Value(_) }) |
    (float ^^ { AstFloat32Value(_) }) |
    (double ^^ { AstFloat64Value(_) }) |
    ("#null" ^^^ AstNullValue) |
    array |
    aggregate |
    (symbol ^^ { AstSymbolValue(_) })
  }

  def argumentList: Parser[List[AstValue]] = "(" ~> repsep(value, ",") <~ ")"

  def optName: Parser[Symbol] = opt(symbol <~ "=") ^^ {
    case Some(name) => name
    case None => symbolFactory.symbol("tmp$")
  }

  def binop: Parser[BinaryOperator] = {
    ("*" | "/" | "%" | "+" | "-" | "<<" | ">>" | ">>>" | "&" | "^" | "|") ^^ {
      BinaryOperator.fromString(_)
    }
  }

  def relop: Parser[RelationalOperator] = {
    ("<" | "<=" | ">" | ">=" | "==" | "!=") ^^ { RelationalOperator.fromString(_) }
  }

  def addressInst: Parser[AstAddressInstruction] = {
    "#address" ~> optName ~ value ~ ("," ~> rep1sep(value, ",")) ^^ {
      case n ~ b ~ is => AstAddressInstruction(n, b, is)
    }
  }

  def assignInst: Parser[AstAssignInstruction] = {
    "#assign" ~> optName ~ value ^^ {
      case n ~ v => AstAssignInstruction(n, v)
    }
  }

  def binopInst: Parser[AstBinaryOperatorInstruction] = {
    "#binop" ~> optName ~ value ~ binop ~ value ^^ {
      case n ~ l ~ op ~ r => AstBinaryOperatorInstruction(n, op, l, r)
    }
  }

  def branchInst: Parser[AstBranchInstruction] = {
    "#branch" ~> optName ~ symbol ~ argumentList ^^ {
      case n ~ v ~ a => AstBranchInstruction(n, v, a)
    }
  }

  def condInst: Parser[AstConditionalBranchInstruction] = {
    "#cond" ~> optName ~ (value <~ "?") ~
      symbol ~ (argumentList <~ ":") ~ symbol ~ argumentList ^^ {
      case  n ~ c ~ t ~ ta ~ f ~ fa => AstConditionalBranchInstruction(n, c, t, ta, f, fa)
    }
  }

  def floatExtendInst: Parser[AstFloatExtendInstruction] = {
    "#fextend" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstFloatExtendInstruction(n, v, t)
    }
  }

  def floatToIntInst: Parser[AstFloatToIntegerInstruction] = {
    "#ftoi" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstFloatToIntegerInstruction(n, v, t)
    }
  }

  def floatTruncateInst: Parser[AstFloatTruncateInstruction] = {
    "#ftruncate" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstFloatTruncateInstruction(n, v, t)
    }
  }

  def heapAllocateInst: Parser[AstHeapAllocateInstruction] = {
    "#heap" ~> (symbol <~ ":") ~ ty ^^ {
      case n ~ t => AstHeapAllocateInstruction(n, t)
    }
  }

  def heapAllocateArrayInst: Parser[AstHeapAllocateArrayInstruction] = {
    "#heaparray" ~> optName ~ value ~ ("*" ~> ty) ^^ {
      case n ~ c ~ t => AstHeapAllocateArrayInstruction(n, c, t)
    }
  }

  def intSignExtendInst: Parser[AstIntegerSignExtendInstruction] = {
    "#isextend" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstIntegerSignExtendInstruction(n, v, t)
    }
  }

  def intToFloatInst: Parser[AstIntegerToFloatInstruction] = {
    "#itof" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstIntegerToFloatInstruction(n, v, t)
    }
  }

  def intTruncateInst: Parser[AstIntegerTruncateInstruction] = {
    "#itruncate" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstIntegerTruncateInstruction(n, v, t)
    }
  }

  def intZeroExtendInst: Parser[AstIntegerZeroExtendInstruction] = {
    "#izextend" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstIntegerZeroExtendInstruction(n, v, t)
    }
  }

  def intrinsicCallInst: Parser[AstIntrinsicCallInstruction] = {
    "#intrinsic" ~> optName ~ symbol ~ argumentList ^^ {
      case n ~ t ~ a => AstIntrinsicCallInstruction(n, t, a)
    }
  }

  def loadInst: Parser[AstLoadInstruction] = {
    "#load" ~> optName ~ ("*" ~> value) ^^ {
      case n ~ v => AstLoadInstruction(n, v)
    }
  }

  def loadElementInst: Parser[AstLoadElementInstruction] = {
    "#loadelement" ~> optName ~ value ~ ("," ~> rep1sep(value, ",")) ^^ {
      case n ~ b ~ is => AstLoadElementInstruction(n, b, is)
    }
  }

  def relopInst: Parser[AstRelationalOperatorInstruction] = {
    "#relop" ~> optName ~ value ~ relop ~ value ^^ {
      case n ~ l ~ op ~ r => AstRelationalOperatorInstruction(n, op, l, r)
    }
  }

  def returnInst: Parser[AstReturnInstruction] = {
    "#return" ~> optName ~ value ^^ { 
      case n ~ v => AstReturnInstruction(n, v) }
  }

  def stackAllocateInst: Parser[AstStackAllocateInstruction] = {
    "#stack" ~> (symbol <~ ":") ~ ty ^^ {
      case n ~ t => AstStackAllocateInstruction(n, t)
    }
  }

  def stackAllocateArrayInst: Parser[AstStackAllocateArrayInstruction] = {
    "#stackarray" ~> optName ~ value ~ ("*" ~> ty) ^^ {
      case n ~ c ~ t => AstStackAllocateArrayInstruction(n, c, t)
    }
  }

  def staticCallInst: Parser[AstStaticCallInstruction] = {
    "#scall" ~> optName ~ symbol ~ argumentList ^^ {
      case n ~ t ~ a => AstStaticCallInstruction(n, t, a)
    }
  }

  def storeInst: Parser[AstStoreInstruction] = {
    "#store" ~> optName ~ ("*" ~> value) ~ ("<-" ~> value) ^^ {
      case n ~ p ~ v => AstStoreInstruction(n, p, v)
    }
  }

  def storeElementInst: Parser[AstStoreElementInstruction] = {
    "#storeelement" ~> optName ~ value ~ ("," ~> rep1sep(value, ",")) ~ 
      ("<-" ~> value) ^^ {
        case n ~ b ~ is ~ v => AstStoreElementInstruction(n, b, is, v)
      }
  }

  def upcastInst: Parser[AstUpcastInstruction] = {
    "#upcast" ~> optName ~ value ~ (":" ~> ty) ^^ {
      case n ~ v ~ t => AstUpcastInstruction(n, v, t)
    }
  }

  def instruction: Parser[AstInstruction] = {
    addressInst |
    assignInst |
    binopInst |
    branchInst |
    condInst |
    floatExtendInst |
    floatToIntInst |
    floatTruncateInst |
    heapAllocateInst |
    heapAllocateArrayInst |
    intSignExtendInst |
    intToFloatInst |
    intTruncateInst |
    intZeroExtendInst |
    intrinsicCallInst |
    loadInst |
    loadElementInst |
    relopInst |
    returnInst |
    stackAllocateInst |
    stackAllocateArrayInst |
    staticCallInst |
    storeInst |
    storeElementInst |
    upcastInst
  }

  def parameter: Parser[AstParameter] = {
    (symbol <~ ":") ~ ty ^^ { case name ~ t => AstParameter(name, t) }
  }

  def parameterList: Parser[List[AstParameter]] = {
    "(" ~> repsep(parameter, ",") <~ ")"
  }

  def body[T](parser: Parser[T]): Parser[List[T]] = {
    opt("{" ~> rep(parser) <~ "}") ^^ { _.getOrElse(Nil) }
  }

  def block: Parser[AstBlock] = {
    "#block" ~> symbol ~ parameterList ~ body(instruction) ^^ { 
      case name ~ params ~ body => AstBlock(name, params, body)
    }
  }

  def field: Parser[AstField] = {
    "#field" ~> symbol ~ (":" ~> ty) ^^ {
      case name ~ ty => AstField(name, ty)
    }
  }

  def global: Parser[AstGlobal] = {
    "#global" ~> symbol ~ (":" ~> ty) ~ opt("=" ~> value) ^^ { 
      case name ~ t ~ iv => AstGlobal(name, t, iv)
    }
  }

  def function: Parser[AstFunction] = {
    "#function" ~> symbol ~ parameterList ~ (":" ~> ty) ~ body(block) ^^ {
        case name ~ params ~ retTy ~ blocks => {
          AstFunction(name, retTy, params, blocks)
        }
      }
  }

  def struct: Parser[AstStruct] = {
    "#struct" ~> symbol ~ body(field) ^^ {
        case name ~ fields => AstStruct(name, fields)
      }
  }  

  def definition: Parser[AstDefinition] = global | function | struct

  def header: Parser[(Symbol, ModuleType, Version, Option[File], List[ModuleDependency], List[File], Boolean, Boolean)] = {
    opt("#name" ~> symbol) ~
      opt("#type" ~> ("#intermediate" | "#library" | "#program")) ~
      opt("#version" ~> version) ~
      opt("#filename" ~> string) ~
      opt("#dependencies" ~> repsep(dependency, ",")) ~
      opt("#searchpaths" ~> repsep(string, ",")) ~
      opt("#is64bit" ~> boolean) ~
      opt("#isSafe" ~> boolean) ^^ {
        case n ~ t ~ v ~ f ~ d ~ s ~ b ~ is => {
          val defaultValues = new Module
          val name = n.getOrElse(defaultValues.name)
          val ty = t match {
            case Some("#intermediate") => ModuleType.INTERMEDIATE
            case Some("#library") => ModuleType.LIBRARY
            case Some("#program") => ModuleType.PROGRAM
            case None => defaultValues.ty
            case _ => throw new AssertionError("Invalid module type")
          }
          val version = v.getOrElse(defaultValues.version)
          val filename = f.map(new File(_))
          val dependencies = d.getOrElse(defaultValues.dependencies)
          val searchPaths = s.map(_.map(new File(_))).getOrElse(defaultValues.searchPaths)
          val is64Bit = b.getOrElse(defaultValues.is64Bit)
          val isSafe = b.getOrElse(defaultValues.isSafe)
          (name, ty, version, filename, dependencies, searchPaths, is64Bit, isSafe)
        }
      }
  }

  def module(file: Option[File]): Parser[AstModule] = header ~ rep(definition) ^^ { 
    case (n, t, v, f, d, s, b, is) ~ definitions => {
      val moduleFile = f match {
        case Some(_) => f
        case None => file
      }
      new AstModule(n, t, v, moduleFile, d, s, b, is, definitions)
    }
  }

  implicit def reserved(r: String): Parser[String] = elem(ReservedToken(r)) ^^^ r

  def test(input: String) = {
    val reader = new AstLexer.Scanner(input)
    val result = phrase(module(None))(reader)
    result match {
      case Success(ast, _) => ast
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}
