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
    "define" ~> ty ~ globalSymbol ~ 
      ("(" ~> repsep(defnParameter, ",") <~ ")") ~ rep(attribute) ~
      ("{" ~> rep1(block) <~ "}") ^^ {
      case rty ~ n ~ ps ~ as ~ bs => Function(n, rty, as, ps, bs)
    }
  }

  def defnParameter: Parser[Parameter] = {
    ty ~ rep(attribute) ~ localSymbol ^^ { case t ~ as ~ n => Parameter(n, t, as) }
  }

  def attribute: Parser[Attribute] = {
    import Attribute._
    "nounwind" ^^^ NOUNWIND
  }

  def block: Parser[Block] = {
    label ~ rep1(instruction) ^^ {
      case n ~ is => Block(n, is)
    }
  }

  def instruction: Parser[Instruction] = {
    allocaInst       |
    bitcastInst      |
    branchInst       |
    extractvalueInst |
    insertvalueInst  |
    loadInst         |
    phiInst          |
    retInst          |
    storeInst        |
    unreachableInst
  }

  def allocaInst: Parser[Instruction] = {
    (localSymbol <~ "=" <~ "alloca") ~ ty ~ opt("," ~> value) ^^ {
      case n ~ t ~ Some(c) => AllocaArrayInstruction(n, t, c)
      case n ~ t ~ None => AllocaInstruction(n, t)
    }
  }

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

  def insertvalueInst: Parser[InsertValueInstruction] = {
    (localSymbol <~ "=" <~ "insertvalue") ~ (value <~ ",") ~ (value <~ ",") ~
      rep1sep(value, ",") ^^ {
        case n ~ b ~ v ~ is => InsertValueInstruction(n, b, v, is)
      }
  }      

  def loadInst: Parser[LoadInstruction] = {
    (localSymbol <~ "=" <~ "load") ~ value ~ opt("," ~> alignment) ^^ {
      case n ~ p ~ a => LoadInstruction(n, p, a)
    }
  }

  def phiInst: Parser[PhiInstruction] = {
    def phiEntries(intro: String ~ Type) = {
      val name ~ ty = intro
      rep1sep("[" ~> (untypedValue(ty) <~ ",") ~ localSymbol <~ "]", ",") ^^ { entries =>
        PhiInstruction(name, ty, entries.map { case v ~ l => (v, l) })
      }
    }
          
    ((localSymbol <~ "=" <~ "phi") ~ ty) >> phiEntries
  }

  def retInst: Parser[ReturnInstruction] = {
    ("ret" ~> value) ^^ { case v => ReturnInstruction(v) }
  }

  def storeInst: Parser[StoreInstruction] = {
    "store" ~> (value <~ ",") ~ value ~ opt("," ~> alignment) ^^ {
      case v ~ p ~ a => StoreInstruction(v, p, a)
    }
  }

  def unreachableInst: Parser[Instruction] = {
    "unreachable" ^^^ UnreachableInstruction
  }

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
    structValue(ty) |
    ((localSymbol | globalSymbol) ^^ { case n => DefinedValue(n, ty) })
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
  
  def ty: Parser[Type] = {
    def basicTy = {
      intType | 
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