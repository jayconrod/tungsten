package tungsten.llvm

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

object Parser extends Parsers with ImplicitConversions {
  type Elem = AnyRef

  implicit def reserved(r: String): Parser[String] = elem(ReservedToken(r)) ^^^ r

  def module: Parser[Module] = {
    targetDataLayout ~ targetTriple ^^ {
      case dl ~ t => new Module(dl, t, Map[String, Definition]())
    }
  }

  def targetDataLayout: Parser[Option[String]] = {
    opt("target" ~> "datalayout" ~> "=" ~> string)
  }

  def targetTriple: Parser[Option[String]] = {
    opt("target" ~> "triple" ~> "=" ~> string)
  }

  def instruction: Parser[Instruction] = {
    allocaInst  |
    bitcastInst |
    branchInst  |
    loadInst    |
    phiInst     |
    retInst     |
    storeInst
  }

  def allocaInst: Parser[AllocaInstruction] = {
    (localSymbol <~ "=" <~ "alloca") ~ ty ^^ {
      case n ~ t => AllocaInstruction(n, t)
    }
  }

  def bitcastInst: Parser[BitcastInstruction] = {
    (localSymbol <~ "=" <~ "bitcast") ~ (value <~ "to") ~ ty ^^ {
      case n ~ v ~ t => BitcastInstruction(n, v, t)
    }
  }

  def branchInst: Parser[BranchInstruction] = {
    ("br" ~> value) ^^ { case l => BranchInstruction(l) }
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

  def alignment: Parser[Int] = {
    "align" ~> integer ^^ { _.toInt }
  }

  def value: Parser[Value] = {
    ty >> untypedValue
  }

  def untypedValue(ty: Type): Parser[Value] = {
    acceptMatch("integer", { case t: IntToken if ty.isInstanceOf[IntType] => 
      IntValue(t.value, ty.asInstanceOf[IntType].width)
    }) |
    ((localSymbol | globalSymbol) ^^ { case n => DefinedValue(n, ty) })
  }        

  def ty: Parser[Type] = {
    def basicTy = {
      intType | 
      ("void" ^^^ VoidType) |
      ("label" ^^^ LabelType)
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

  def test(input: String) = {
    val reader = new Lexer.Scanner(input)
    val result = phrase(module)(reader)
    result match {
      case Success(ast, _) => ast
      case error: NoSuccess => throw new RuntimeException(error.msg)
    }
  }
}