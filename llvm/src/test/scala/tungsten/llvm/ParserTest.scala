package tungsten.llvm

import org.junit.Test
import org.junit.Assert._

class ParserTest {
  def test[T](input: String, parser: Parser.Parser[T], expected: T) {
    val scanner = new Lexer.Scanner(input)
    val result = Parser.phrase(parser)(scanner)
    result match {
      case Parser.Success(ast, _) => assertEquals(expected, ast)
      case parseError: Parser.NoSuccess => fail(parseError.msg)
    }
  }

  def testModule(input: String, expected: Module) {
    test(input, Parser.module, expected)
  }

  @Test
  def empty {
    testModule("", new Module(None, None, Map[String, Definition]()))
  }

  @Test
  def whitespace {
    testModule(" \t\n", new Module(None, None, Map[String, Definition]()))
  }

  @Test
  def headers {
    val dataLayoutString = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128"
    val tripleString = "x86_64-linux-gnu"
    testModule("; ModuleID = '<stdin>'\n" +
               "target datalayout = \"" + dataLayoutString + "\"\n" +
               "target triple = \"" + tripleString + "\"\n",
               new Module(Some(dataLayoutString), Some(tripleString), Map[String, Definition]()))
  }

  @Test
  def localSymbol {
    test("%test", Parser.localSymbol, "%test")
    test("%0", Parser.localSymbol, "%0")
  }

  @Test
  def globalSymbol {
    val input = "@test"
    test(input, Parser.globalSymbol, input)
  }

  @Test
  def voidType {
    test("void", Parser.ty, VoidType)
  }

  @Test
  def labelType {
    test("label", Parser.ty, LabelType)
  }

  @Test
  def pointerType {
    test("i32**", Parser.ty, PointerType(PointerType(IntType(32))))
  }

  @Test
  def intType {
    test("i32", Parser.ty, IntType(32))
    test("i1", Parser.ty, IntType(1))
    test("i128", Parser.ty, IntType(128))
  }

  @Test
  def intValue {
    test("i32 -123", Parser.value, IntValue(-123L, 32))
  }

  @Test
  def definedValue {
    test("i32 %x", Parser.value, DefinedValue("%x", IntType(32)))
    test("i32 @x", Parser.value, DefinedValue("@x", IntType(32)))
  }

  @Test
  def allocaInst {
    test("%0 = alloca i32", Parser.instruction, AllocaInstruction("%0", IntType(32)))
  }

  @Test
  def bitcastInst {
    test("%0 = bitcast i32 0 to i32", Parser.instruction,
         BitcastInstruction("%0", IntValue(0L, 32), IntType(32)))
  }

  @Test
  def branchInst {
    test("br label %bb0", Parser.instruction,
         BranchInstruction(DefinedValue("%bb0", LabelType)))
  }

  @Test
  def loadInst {
    test("%0 = load i32* %p, align 4", Parser.instruction,
         LoadInstruction("%0", DefinedValue("%p", PointerType(IntType(32))), Some(4)))
  }

  @Test
  def phiInst {
    test("%x = phi i32 [0, %bb0], [1, %bb1]", Parser.instruction,
         PhiInstruction("%x", IntType(32), 
                        List((IntValue(0L, 32), "%bb0"), (IntValue(1L, 32), "%bb1"))))
  }

  @Test
  def retInst {
    test("ret i32 0", Parser.instruction, ReturnInstruction(IntValue(0L, 32)))
  }

  @Test
  def storeInst {
    test("store i32 %v, i32* %p, align 4", Parser.instruction,
         StoreInstruction(DefinedValue("%v", IntType(32)),
                          DefinedValue("%p", PointerType(IntType(32))),
                          Some(4)))
  }
}
