package tungsten

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

object AstParser extends Parsers with ImplicitConversions {
  type Elem = Token

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

  def ty: Parser[AstType] = {
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
    val byte = accept("byte", { case ByteToken(b) => b })
    val short = accept("short", { case ShortToken(s) => s })
    val int = accept("int", { case IntToken(i) => i })
    val long = accept("long", { case LongToken(l) => l })
    val float = accept("float", { case Float32Token(f) => f })
    val double = accept("double", { case Float64Token(d) => d })

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
    (symbol ~ location ^^ flatten2(AstSymbolValue(_, _)))
  }

  def argumentList: Parser[List[AstValue]] = "(" ~> repsep(value, ",") <~ ")"

  def binop: Parser[BinaryOperator] = {
    ("*" | "/" | "%" | "+" | "-" | "<<" | ">>" | ">>>" | "&" | "^" | "|") ^^ {
      BinaryOperator.fromString(_)
    }
  }

  def relop: Parser[RelationalOperator] = {
    ("<" | "<=" | ">" | ">=" | "==" | "!=") ^^ { RelationalOperator.fromString(_) }
  }

  def assignInst: Parser[AstAssignInstruction] = {
    "#assign" ~> location ~ (symbol <~ "=") ~ value ^^ {
      case l ~ n ~ v => AstAssignInstruction(n, v, l)
    }
  }

  def binopInst: Parser[AstBinaryOperatorInstruction] = {
    "#binop" ~> location ~ (symbol <~ "=") ~ value ~ binop ~ value ^^ {
      case loc ~ n ~ l ~ op ~ r => AstBinaryOperatorInstruction(n, op, l, r, loc)
    }
  }

  def branchInst: Parser[AstBranchInstruction] = {
    "#branch" ~> location ~ (symbol <~ "=") ~ symbol ~ argumentList ^^ {
      case l ~ n ~ v ~ a => AstBranchInstruction(n, v, a, l)
    }
  }

  def condInst: Parser[AstConditionalBranchInstruction] = {
    "#cond" ~> location ~ (symbol <~ "=") ~ 
      (value <~ "?") ~ (symbol <~ ":") ~ symbol ~ argumentList ^^ {
      case l ~ n ~ c ~ t ~ f ~ a => AstConditionalBranchInstruction(n, c, t, f, a, l)
    }
  }

  def gloadInst: Parser[AstGlobalLoadInstruction] = {
    "#gload" ~> location ~ (symbol <~ "=") ~ symbol ^^ {
      case l ~ n ~ v => AstGlobalLoadInstruction(n, v, l)
    }
  }

  def gstoreInst: Parser[AstGlobalStoreInstruction] = {
    "#gstore" ~> location ~ (symbol <~ "=") ~ (symbol <~ "<-") ~ value ^^ {
      case l ~ n ~ g ~ v => AstGlobalStoreInstruction(n, g, v, l)
    }
  }

  def indirectCallInst: Parser[AstIndirectCallInstruction] = {
    "#icall" ~> location ~ (symbol <~ "=") ~ value ~ argumentList ^^ {
      case l ~ n ~ t ~ a => AstIndirectCallInstruction(n, t, a, l)
    }
  }

  def intrinsicCallInst: Parser[AstIntrinsicCallInstruction] = {
    "#intrinsic" ~> location ~ (symbol <~ "=") ~ symbol ~ argumentList ^^ {
      case l ~ n ~ t ~ a => AstIntrinsicCallInstruction(n, t, a, l)
    }
  }

  def relopInst: Parser[AstRelationalOperatorInstruction] = {
    "#relop" ~> location ~ (symbol <~ "=") ~ value ~ relop ~ value ^^ {
      case loc ~ n ~ l ~ op ~ r => AstRelationalOperatorInstruction(n, op, l, r, loc)
    }
  }

  def returnInst: Parser[AstReturnInstruction] = {
    "#return" ~> location ~ (symbol <~ "=") ~ value ^^ { 
      case l ~ n ~ v => AstReturnInstruction(n, v, l) }
  }

  def staticCallInst: Parser[AstStaticCallInstruction] = {
    "#scall" ~> location ~ (symbol <~ "=") ~ symbol ~ argumentList ^^ {
      case l ~ n ~ t ~ a => AstStaticCallInstruction(n, t, a, l)
    }
  }

  def upcastInst: Parser[AstUpcastInstruction] = {
    "#upcast" ~> location ~ (symbol <~ "=") ~ value ~ (":" ~> ty) ^^ {
      case l ~ n ~ v ~ t => AstUpcastInstruction(n, v, t, l)
    }
  }

  def instruction: Parser[AstInstruction] = {
    assignInst |
    binopInst |
    branchInst |
    condInst |
    gloadInst |
    gstoreInst |
    indirectCallInst |
    intrinsicCallInst |
    relopInst |
    returnInst |
    staticCallInst |
    upcastInst
  }

  def parameter: Parser[AstParameter] = {
    symbol ~ location ~ ":" ~ ty ^^ { case name ~ loc ~ _ ~ t => AstParameter(name, t, loc) }
  }

  def parameterList: Parser[List[AstParameter]] = {
    "(" ~> repsep(parameter, ",") <~ ")"
  }

  def block: Parser[AstBlock] = {
    "#block" ~> location ~ symbol ~ parameterList ~ ("{" ~> rep(instruction) <~ "}") ^^ { 
      case loc ~ name ~ params ~ body => AstBlock(name, params, body, loc)
    }
  }

  def typeParameter: Parser[AstTypeParameter] = {
    val subtype = opt("<:" ~> ty)
    val supertype = opt(">:" ~> ty)
    symbol ~ location ~ subtype ~ supertype ^^ {
      case name ~ loc ~ sub ~ sup => AstTypeParameter(name, sub, sup, loc)
    }
  }

  def typeParameterList: Parser[List[AstTypeParameter]] = {
    opt("[" ~> rep1sep(typeParameter, ",") <~ "]") ^^ {
      case Some(params) => params
      case None => Nil
    }
  }

  def superclass: Parser[Option[AstType]] = opt("<:" ~> ty)

  def interfaces: Parser[List[AstType]] = {
    opt(":" ~> rep1sep(ty, ",")) ^^ { _.getOrElse(Nil) }
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
    "#function" ~> location ~ symbol ~ typeParameterList ~ parameterList ~ (":" ~> ty) ~
      opt("{" ~> rep(block) <~ "}") ^^ {
        case loc ~ name ~ tyParams ~ params ~ retTy ~ body => {
          val blocks = body.getOrElse(Nil)
          AstFunction(name, retTy, tyParams, params, blocks, loc)
        }
      }
  }

  def struct: Parser[AstStruct] = {
    "#struct" ~> location ~ symbol ~ typeParameterList ~ 
      ("{" ~> rep1sep(field, ",") <~ "}") ^^ {
        case loc ~ name ~ tyParams ~ fields => AstStruct(name, tyParams, fields, loc)
      }
  }  

  def clas: Parser[AstClass] = {
    def fields: Parser[List[AstField]] = {
      "#fields" ~> "{" ~> repsep(field, ",") <~ "}"
    }
    def methods: Parser[List[AstFunction]] = {
      "#methods" ~> "{" ~> repsep(function, ",") <~ "}"
    }
    
    "#class" ~> location ~ symbol ~ typeParameterList ~ superclass ~ interfaces ~ 
      ("{" ~> fields ~ methods <~ "}") ^^ { 
        case loc ~ name ~ tyParams ~ sup ~ is ~ (fs ~ ms) => {
          AstClass(name, tyParams, sup, is, fs, ms, loc)
        }
      }
  }

  def iface: Parser[AstInterface] = {
    "#interface" ~> location ~ symbol ~ typeParameterList ~ superclass ~ interfaces ~
      ("{" ~> repsep(function, ",") <~ "}") ^^ {
        case loc ~ name ~ tyParams ~ sup ~ is ~ ms => {
          AstInterface(name, tyParams, sup, is, ms, loc)
        }
      }
  }

  def definition: Parser[AstDefinition] = global | function | struct | clas | iface

  def module: Parser[AstModule] = rep(definition) ^^ { AstModule(_) }

  implicit def reserved(r: String): Parser[String] = elem(ReservedToken(r)) ^^^ r

  def test(input: String) = {
    val reader = new AstLexer.Scanner(input)
    val result = phrase(module)(reader)
    result match {
      case Success(ast, _) => ast
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}
