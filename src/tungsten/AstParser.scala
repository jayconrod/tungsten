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

  def location: Parser[Location] = {
    val locParser = elem("location", _.isInstanceOf[LocationToken]) ^^ {
      case LocationToken(loc) => loc
      case _ => throw new AssertionError
    }
    opt(locParser) ^^ {
      case Some(l) => l
      case None => Nowhere
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
      "[" ~> (size <~ "*") ~ (ty <~ "]") ~ location ^^ { 
        case s ~ t ~ l => AstArrayType(s, t, l)
      }
    }
    def elementTy: Parser[AstType] = {
      ("#unit" ~> location ^^ { AstUnitType(_) }) |
      ("#boolean" ~> location ^^ { AstBooleanType(_) }) |
      ("#int8" ~> location ^^ { AstIntType(8, _) }) |
      ("#int16" ~> location ^^ { AstIntType(16, _) }) |
      ("#int32" ~> location ^^ { AstIntType(32, _) }) |
      ("#int64" ~> location ^^ { AstIntType(64, _) }) |
      ("#float32" ~> location ^^ { AstFloatType(32, _) }) |
      ("#float64" ~> location ^^ { AstFloatType(64, _) }) |
      ("#null" ~> location ^^ { AstNullType(_) }) |
      arrayTy |
      (symbol ~ typeArguments ~ location ^^ { 
        case name ~ args ~ loc => AstClassType(name, args, loc)
      })
    }
    def makePointerType(ety: AstType, stars: List[Location]): AstType = {
      stars match {
        case Nil => ety
        case l :: ls => {
          val pty = AstPointerType(ety, l)
          makePointerType(pty, ls)
        }
        case _ => throw new RuntimeException("token must be location")
      }
    }

    elementTy ~ rep("*" ~> location) ^^ { 
      case ety ~ stars => makePointerType(ety, stars)
    }   
  }

  def typeArguments: Parser[List[AstType]] = {
    opt("[" ~> rep1sep(ty, ",") <~ "]") ^^ {
      case Some(types) => types
      case None => Nil
    }
  }

  def value: Parser[AstValue] = {
    def byte = accept("byte", { case ByteToken(b) => b })
    def short = accept("short", { case ShortToken(s) => s })
    def int = accept("int", { case IntToken(i) => i })
    def long = accept("long", { case LongToken(l) => l })
    def float = accept("float", { case Float32Token(f) => f })
    def double = accept("double", { case Float64Token(d) => d })

    def array = {
      "[" ~> (ty <~ ":") ~ (repsep(value, ",") <~"]") ~ location ^^ 
        flatten3(AstArrayValue(_, _, _))
    }

    def aggregate = {
      "{" ~> (symbol <~ ":") ~ (repsep(value, ",") <~ "}") ~ location ^^
        flatten3(AstAggregateValue(_, _, _))
    }

    ("()" ~> location ^^ { AstUnitValue(_) }) |
    ("#true" ~> location ^^ { AstBooleanValue(true, _) }) |
    ("#false" ~> location ^^ { AstBooleanValue(false, _) }) |
    (byte ~ location ^^ flatten2(AstInt8Value(_, _))) |
    (short ~ location ^^ flatten2(AstInt16Value(_, _))) |
    (int ~ location ^^ flatten2(AstInt32Value(_, _))) |
    (long ~ location ^^ flatten2(AstInt64Value(_, _))) |
    (float ~ location ^^ flatten2(AstFloat32Value(_, _))) |
    (double ~ location ^^ flatten2(AstFloat64Value(_, _))) |
    ("#null" ~> location ^^ { AstNullValue(_) }) |
    array |
    aggregate |
    (symbol ~ location ^^ flatten2(AstSymbolValue(_, _)))
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
    "#address" ~> location ~ optName ~ value ~ ("," ~> rep1sep(value, ",")) ^^ {
      case l ~ n ~ b ~ is => AstAddressInstruction(n, b, is, l)
    }
  }

  def assignInst: Parser[AstAssignInstruction] = {
    "#assign" ~> location ~ optName ~ value ^^ {
      case l ~ n ~ v => AstAssignInstruction(n, v, l)
    }
  }

  def binopInst: Parser[AstBinaryOperatorInstruction] = {
    "#binop" ~> location ~ optName ~ value ~ binop ~ value ^^ {
      case loc ~ n ~ l ~ op ~ r => AstBinaryOperatorInstruction(n, op, l, r, loc)
    }
  }

  def branchInst: Parser[AstBranchInstruction] = {
    "#branch" ~> location ~ optName ~ symbol ~ argumentList ^^ {
      case l ~ n ~ v ~ a => AstBranchInstruction(n, v, a, l)
    }
  }

  def condInst: Parser[AstConditionalBranchInstruction] = {
    "#cond" ~> location ~ optName ~ (value <~ "?") ~
      symbol ~ (argumentList <~ ":") ~ symbol ~ argumentList ^^ {
      case l ~ n ~ c ~ t ~ ta ~ f ~ fa => AstConditionalBranchInstruction(n, c, t, ta, f, fa, l)
    }
  }

  def floatExtendInst: Parser[AstFloatExtendInstruction] = {
    "#fextend" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstFloatExtendInstruction(n, v, t, l)
    }
  }

  def floatToIntInst: Parser[AstFloatToIntegerInstruction] = {
    "#ftoi" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstFloatToIntegerInstruction(n, v, t, l)
    }
  }

  def floatTruncateInst: Parser[AstFloatTruncateInstruction] = {
    "#ftruncate" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstFloatTruncateInstruction(n, v, t, l)
    }
  }

  def heapAllocateInst: Parser[AstHeapAllocateInstruction] = {
    "#heap" ~> location ~ (symbol <~ ":") ~ ty ^^ {
      case l ~ n ~ t => AstHeapAllocateInstruction(n, t, l)
    }
  }

  def heapAllocateArrayInst: Parser[AstHeapAllocateArrayInstruction] = {
    "#heaparray" ~> location ~ optName ~ value ~ ("*" ~> ty) ^^ {
      case l ~ n ~ c ~ t => AstHeapAllocateArrayInstruction(n, c, t, l)
    }
  }

  def indirectCallInst: Parser[AstIndirectCallInstruction] = {
    "#icall" ~> location ~ optName ~ value ~ argumentList ^^ {
      case l ~ n ~ t ~ a => AstIndirectCallInstruction(n, t, a, l)
    }
  }

  def intSignExtendInst: Parser[AstIntegerSignExtendInstruction] = {
    "#isextend" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstIntegerSignExtendInstruction(n, v, t, l)
    }
  }

  def intToFloatInst: Parser[AstIntegerToFloatInstruction] = {
    "#itof" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstIntegerToFloatInstruction(n, v, t, l)
    }
  }

  def intTruncateInst: Parser[AstIntegerTruncateInstruction] = {
    "#itruncate" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstIntegerTruncateInstruction(n, v, t, l)
    }
  }

  def intZeroExtendInst: Parser[AstIntegerZeroExtendInstruction] = {
    "#izextend" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstIntegerZeroExtendInstruction(n, v, t, l)
    }
  }

  def intrinsicCallInst: Parser[AstIntrinsicCallInstruction] = {
    "#intrinsic" ~> location ~ optName ~ symbol ~ argumentList ^^ {
      case l ~ n ~ t ~ a => AstIntrinsicCallInstruction(n, t, a, l)
    }
  }

  def loadInst: Parser[AstLoadInstruction] = {
    "#load" ~> location ~ optName ~ ("*" ~> value) ^^ {
      case l ~ n ~ v => AstLoadInstruction(n, v, l)
    }
  }

  def loadElementInst: Parser[AstLoadElementInstruction] = {
    "#loadelement" ~> location ~ optName ~ value ~ ("," ~> rep1sep(value, ",")) ^^ {
      case l ~ n ~ b ~ is => AstLoadElementInstruction(n, b, is, l)
    }
  }

  def relopInst: Parser[AstRelationalOperatorInstruction] = {
    "#relop" ~> location ~ optName ~ value ~ relop ~ value ^^ {
      case loc ~ n ~ l ~ op ~ r => AstRelationalOperatorInstruction(n, op, l, r, loc)
    }
  }

  def returnInst: Parser[AstReturnInstruction] = {
    "#return" ~> location ~ optName ~ value ^^ { 
      case l ~ n ~ v => AstReturnInstruction(n, v, l) }
  }

  def stackAllocateInst: Parser[AstStackAllocateInstruction] = {
    "#stack" ~> location ~ (symbol <~ ":") ~ ty ^^ {
      case l ~ n ~ t => AstStackAllocateInstruction(n, t, l)
    }
  }

  def stackAllocateArrayInst: Parser[AstStackAllocateArrayInstruction] = {
    "#stackarray" ~> location ~ optName ~ value ~ ("*" ~> ty) ^^ {
      case l ~ n ~ c ~ t => AstStackAllocateArrayInstruction(n, c, t, l)
    }
  }

  def staticCallInst: Parser[AstStaticCallInstruction] = {
    "#scall" ~> location ~ optName ~ symbol ~ argumentList ^^ {
      case l ~ n ~ t ~ a => AstStaticCallInstruction(n, t, a, l)
    }
  }

  def storeInst: Parser[AstStoreInstruction] = {
    "#store" ~> location ~ optName ~ ("*" ~> value) ~ ("<-" ~> value) ^^ {
      case l ~ n ~ p ~ v => AstStoreInstruction(n, p, v, l)
    }
  }

  def storeElementInst: Parser[AstStoreElementInstruction] = {
    "#storeelement" ~> location ~ optName ~ value ~ ("," ~> rep1sep(value, ",")) ~ 
      ("<-" ~> value) ^^ {
        case l ~ n ~ b ~ is ~ v => AstStoreElementInstruction(n, b, is, v, l)
      }
  }

  def upcastInst: Parser[AstUpcastInstruction] = {
    "#upcast" ~> location ~ optName ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstUpcastInstruction(n, v, t, l)
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
    indirectCallInst |
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
    symbol ~ location ~ ":" ~ ty ^^ { case name ~ loc ~ _ ~ t => AstParameter(name, t, loc) }
  }

  def parameterList: Parser[List[AstParameter]] = {
    "(" ~> repsep(parameter, ",") <~ ")"
  }

  def body[T](parser: Parser[T]): Parser[List[T]] = {
    opt("{" ~> rep(parser) <~ "}") ^^ { _.getOrElse(Nil) }
  }

  def block: Parser[AstBlock] = {
    "#block" ~> location ~ symbol ~ parameterList ~ body(instruction) ^^ { 
      case loc ~ name ~ params ~ body => AstBlock(name, params, body, loc)
    }
  }

  def field: Parser[AstField] = {
    "#field" ~> location ~ symbol ~ (":" ~> ty) ^^ {
      case loc ~ name ~ ty => AstField(name, ty, loc)
    }
  }

  def global: Parser[AstGlobal] = {
    "#global" ~ location ~ symbol ~ ":" ~ ty ~ opt("=" ~> value) ^^ { 
      case _ ~ loc ~ name ~ _ ~ t ~ iv => AstGlobal(name, t, iv, loc)
    }
  }

  def function: Parser[AstFunction] = {
    "#function" ~> location ~ symbol ~ parameterList ~ (":" ~> ty) ~ body(block) ^^ {
        case loc ~ name ~ params ~ retTy ~ blocks => {
          AstFunction(name, retTy, params, blocks, loc)
        }
      }
  }

  def struct: Parser[AstStruct] = {
    "#struct" ~> location ~ symbol ~ body(field) ^^ {
        case loc ~ name ~ fields => AstStruct(name, fields, loc)
      }
  }  

  def definition: Parser[AstDefinition] = global | function | struct

  def header: Parser[(Symbol, ModuleType, Version, Option[File], List[ModuleDependency], List[File])] = {
    opt("#name" ~> symbol) ~
      opt("#type" ~> ("#intermediate" | "#library" | "#program")) ~
      opt("#version" ~> version) ~
      opt("#filename" ~> string) ~
      opt("#dependencies" ~> repsep(dependency, ",")) ~
      opt("#searchpaths" ~> repsep(string, ",")) ^^ {
        case n ~ t ~ v ~ f ~ d ~ s => {
          val name = n.getOrElse(Symbol("default"))
          val ty = t match {
            case Some("#intermediate") => ModuleType.INTERMEDIATE
            case Some("#library") => ModuleType.LIBRARY
            case Some("#program") => ModuleType.PROGRAM
            case None => ModuleType.INTERMEDIATE
            case _ => throw new AssertionError("Invalid module type")
          }
          val version = v.getOrElse(Version.MIN)
          val filename = f.map(new File(_))
          val dependencies = d.getOrElse(Nil)
          val searchPaths = s.getOrElse(Nil).map(new File(_))
          (name, ty, version, filename, dependencies, searchPaths)
        }
      }
  }

  def module(file: Option[File]): Parser[AstModule] = header ~ rep(definition) ^^ { 
    case (n, t, v, f, d, s) ~ definitions => {
      val moduleFile = f match {
        case Some(_) => f
        case None => file
      }
      new AstModule(n, t, v, moduleFile, d, s, definitions)
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
