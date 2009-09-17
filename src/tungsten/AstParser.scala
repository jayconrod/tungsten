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
    ("#int16" ~> location ^^ { AstIntType(64, _) })
  }

  def value: Parser[AstValue] = {
    val integer = elem("integer", _.isInstanceOf[IntegerToken]) ^^ {
      case IntegerToken(i) => i
      case _ => throw new AssertionError
    }
    ("()" ~> location ^^ { AstUnitValue(_) }) |
    (integer ~ location ^^ { flatten2(AstIntValue(_, _)) }) |
    (symbol ~ location ^^ { flatten2(AstSymbolValue(_, _)) })
  }

  def argumentList: Parser[List[AstValue]] = "(" ~> repsep(value, ",") <~ ")"

  def returnInst: Parser[AstReturnInstruction] = {
    "#return" ~> location ~ value ^^ { case l ~ v => AstReturnInstruction(v, l) }
  }

  def branchInst: Parser[AstBranchInstruction] = {
    "#branch" ~> location ~ value ~ argumentList ^^ {
      case l ~ v ~ a => AstBranchInstruction(v, a, l)
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

  def definition: Parser[AstDefinition] = global | function

  def module: Parser[AstModule] = rep(definition) ^^ { AstModule(_) }

  implicit def reserved(r: String): Parser[Token] = elem(ReservedToken(r))

  def test(input: String) = {
    val reader = new AstLexer.Scanner(input)
    val result = phrase(module)(reader)
    result.get
  }
}
