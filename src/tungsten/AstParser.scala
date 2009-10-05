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
    ("#unit" ~> location ^^ { AstUnitType(_) }) |
    ("#int8" ~> location ^^ { AstIntType(8, _) }) |
    ("#int16" ~> location ^^ { AstIntType(16, _) }) |
    ("#int32" ~> location ^^ { AstIntType(32, _) }) |
    ("#int16" ~> location ^^ { AstIntType(64, _) }) |
    (symbol ~ typeArguments ~ location ^^ { 
      case name ~ args ~ loc => AstClassType(name, args, loc)
    })
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

    ("()" ~> location ^^ { AstUnitValue(_) }) |
    (byte ~ location ^^ flatten2(AstInt8Value(_, _))) |
    (short ~ location ^^ flatten2(AstInt16Value(_, _))) |
    (int ~ location ^^ flatten2(AstInt32Value(_, _))) |
    (long ~ location ^^ flatten2(AstInt64Value(_, _))) |
    (symbol ~ location ^^ { flatten2(AstSymbolValue(_, _)) })
  }

  def argumentList: Parser[List[AstValue]] = "(" ~> repsep(value, ",") <~ ")"

  def returnInst: Parser[AstReturnInstruction] = {
    "#return" ~> location ~ (symbol <~ "=") ~ value ^^ { 
      case l ~ n ~ v => AstReturnInstruction(n, v, l) }
  }

  def branchInst: Parser[AstBranchInstruction] = {
    "#branch" ~> location ~ (symbol <~ "=") ~ value ~ argumentList ^^ {
      case l ~ n ~ v ~ a => AstBranchInstruction(n, v, a, l)
    }
  }

  def instruction: Parser[AstInstruction] = {
    returnInst | branchInst
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
      opt("{" ~> rep1sep(block, ",") <~ "}") ^^ {
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

  implicit def reserved(r: String): Parser[Token] = elem(ReservedToken(r))

  def test(input: String) = {
    val reader = new AstLexer.Scanner(input)
    val result = phrase(module)(reader)
    result.get
  }
}
