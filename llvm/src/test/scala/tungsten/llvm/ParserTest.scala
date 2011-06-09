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
  def namedStructType {
    test("%foo", Parser.ty, NamedStructType("%foo"))
  }

  @Test
  def structType {
    test("{i64, i64}", Parser.ty, StructType(List(IntType(64), IntType(64))))
  }

  @Test
  def intValue {
    test("i32 -123", Parser.value, IntValue(-123L, 32))
  }

  @Test
  def floatValue {
    test("float 1.0", Parser.value, FloatValue(1.0, 32))
    test("double 1.0", Parser.value, FloatValue(1.0, 64))
    test("double 1e4", Parser.value, FloatValue(1e4, 64))
  }

  @Test
  def structValue {
    test("{i64, i64} { i64 12, i64 34 }", Parser.value,
         StructValue(List(IntType(64), IntType(64)),
                     List(IntValue(12, 64), IntValue(34, 64))))
  }

  @Test
  def namedStructValue {
    test("%foo { i64 12, i64 34 }", Parser.value,
         NamedStructValue("%foo", List(IntValue(12, 64), IntValue(34, 64))))
  }

  @Test
  def definedValue {
    test("i32 %x", Parser.value, DefinedValue("%x", IntType(32)))
    test("i32 @x", Parser.value, DefinedValue("@x", IntType(32)))
  }

  @Test
  def bitcastValue {
    test("i8* bitcast (i64* %x to i8*)", Parser.value,
         BitCastValue(DefinedValue("%x", PointerType(IntType(64))), PointerType(IntType(8))))
  }

  @Test
  def addInst {
    test("%x = add i64 1, 2", Parser.instruction, 
         AddInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def allocaInst {
    test("%0 = alloca i32", Parser.instruction, AllocaInstruction("%0", IntType(32)))
  }

  @Test
  def allocaArrayInst {
    test("%0 = alloca i32, i32 1", Parser.instruction, 
         AllocaArrayInstruction("%0", IntType(32), IntValue(1, 32)))
  }

  @Test
  def andInst {
    test("%x = and i64 1, 2", Parser.instruction,
         AndInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def asrInst {
    test("%x = asr i64 1, 2", Parser.instruction,
         ArithmeticShiftRightInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def bitcastInst {
    test("%0 = bitcast i32 0 to i32", Parser.instruction,
         BitCastInstruction("%0", IntValue(0L, 32), IntType(32)))
  }

  @Test
  def branchInst {
    test("br label %bb0", Parser.instruction,
         BranchInstruction(DefinedValue("%bb0", LabelType)))
  }

  @Test
  def extractvalueInst {
    test("%0 = extractvalue i32 0, i32 0", Parser.instruction,
         ExtractValueInstruction("%0", IntValue(0, 32), List(IntValue(0, 32))))
  }

  @Test
  def faddInst {
    test("%x = fadd double 1.0, 2.0", Parser.instruction,
         FloatAddInstruction("%x", FloatType(64), FloatValue(1.0, 64), FloatValue(2.0, 64)))
  }

  @Test
  def fcmpInst {
    test("%x = fcmp oeq double 1.0, 2.0", Parser.instruction,
         FloatCompareInstruction("%x", Comparison.OEQ,
                                 FloatType(64), FloatValue(1.0, 64), FloatValue(2.0, 64)))
  }

  @Test
  def fdivInst {
    test("%x = fdiv double 1.0, 2.0", Parser.instruction,
         FloatDivideInstruction("%x", FloatType(64), FloatValue(1.0, 64), FloatValue(2.0, 64)))
  }

  @Test
  def fmulInst {
    test("%x = fmul double 1.0, 2.0", Parser.instruction,
         FloatMultiplyInstruction("%x", FloatType(64), FloatValue(1.0, 64), FloatValue(2.0, 64)))
  }

  @Test
  def fpextInst {
    test("%x = fpext float 1.0 to double", Parser.instruction,
         FloatExtendInstruction("%x", FloatValue(1.0, 32), FloatType(64)))
  }

  @Test
  def fptouiInst {
    test("%x = fptoui float 1.0 to i32", Parser.instruction,
         FloatToUnsignedIntegerInstruction("%x", FloatValue(1.0, 32), IntType(32)))
  }

  @Test
  def fptosiInst {
    test("%x = fptosi float 1.0 to i32", Parser.instruction,
         FloatToSignedIntegerInstruction("%x", FloatValue(1.0, 32), IntType(32)))
  }

  @Test
  def fptruncInst {
    test("%x = fptrunc double 1.0 to float", Parser.instruction,
         FloatTruncateInstruction("%x", FloatValue(1.0, 64), FloatType(32)))
  }

  @Test
  def fremInst {
    test("%x = frem double 1.0, 2.0", Parser.instruction,
         FloatRemainderInstruction("%x", FloatType(64), FloatValue(1.0, 64), FloatValue(2.0, 64)))
  }

  @Test
  def fsubInst {
    test("%x = fsub double 1.0, 2.0", Parser.instruction,
         FloatSubtractInstruction("%x", FloatType(64), FloatValue(1.0, 64), FloatValue(2.0, 64)))
  }

  @Test
  def gepInst {
    test("%x = getelementptr i64* %base, i32 0", Parser.instruction,
         GetElementPointerInstruction("%x", 
                                      DefinedValue("%base", PointerType(IntType(64))),
                                      List(IntValue(0, 32))))
  }

  @Test
  def icmpInst {
    test("%x = icmp eq i64 1, 2", Parser.instruction,
         IntegerCompareInstruction("%x", Comparison.EQ, 
                                   IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def insertvalueInst {
    test("%0 = insertvalue i32 0, i32 0, i32 0", Parser.instruction,
         InsertValueInstruction("%0", IntValue(0, 32), IntValue(0, 32), List(IntValue(0, 32))))
  }

  @Test
  def inttoptrInst {
    test("%x = inttoptr i64 0 to i64*", Parser.instruction,
         IntegerToPointerInstruction("%x", IntValue(0, 64), PointerType(IntType(64))))
  }

  @Test
  def loadInst {
    test("%0 = load i32* %p, align 4", Parser.instruction,
         LoadInstruction("%0", DefinedValue("%p", PointerType(IntType(32))), Some(4)))
  }

  @Test
  def lsrInst {
    test("%x = lsr i64 1, 2", Parser.instruction,
         LogicalShiftRightInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def mulInst {
    test("%x = mul i64 1, 2", Parser.instruction,
         MultiplyInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def orInst {
    test("%x = or i64 1, 2", Parser.instruction,
         OrInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def phiInst {
    test("%x = phi i32 [0, %bb0], [1, %bb1]", Parser.instruction,
         PhiInstruction("%x", IntType(32), 
                        List((IntValue(0L, 32), "%bb0"), (IntValue(1L, 32), "%bb1"))))
  }

  @Test
  def ptrtointInst {
    test("%x = ptrtoint i64* %p to i64", Parser.instruction,
         PointerToIntegerInstruction("%x", DefinedValue("%p", PointerType(IntType(64))),
                                     IntType(64)))
  }

  @Test
  def retInst {
    test("ret i32 0", Parser.instruction, ReturnInstruction(IntValue(0L, 32)))
  }

  @Test
  def sextInst {
    test("%x = sext i32 0 to i64", Parser.instruction,
         SignExtendInstruction("%x", IntValue(0, 32), IntType(64)))
  }

  @Test
  def shlInst {
    test("%x = shl i64 1, 2", Parser.instruction,
         ShiftLeftInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def sitofpInst {
    test("%x = sitofp i64 0 to double", Parser.instruction,
         SignedIntegerToFloatInstruction("%x", IntValue(0, 64), FloatType(64)))
  }

  @Test
  def sdivInst {
    test("%x = sdiv i64 1, 2", Parser.instruction,
         SignedDivideInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def sremInst {
    test("%x = srem i64 1, 2", Parser.instruction,
         SignedRemainderInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def storeInst {
    test("store i32 %v, i32* %p, align 4", Parser.instruction,
         StoreInstruction(DefinedValue("%v", IntType(32)),
                          DefinedValue("%p", PointerType(IntType(32))),
                          Some(4)))
  }

  @Test
  def subInst {
    test("%x = sub i64 1, 2", Parser.instruction,
         SubtractInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def truncInst {
    test("%x = trunc i64 0 to i32", Parser.instruction,
         TruncateInstruction("%x", IntValue(0, 64), IntType(32)))
  }

  @Test
  def uitofpInst {
    test("%x = uitofp i64 0 to double", Parser.instruction,
         UnsignedIntegerToFloatInstruction("%x", IntValue(0, 64), FloatType(64)))
  }

  @Test
  def unreachableInst {
    test("unreachable", Parser.instruction, UnreachableInstruction)
  }

  @Test
  def udivInst {
    test("%x = udiv i64 1, 2", Parser.instruction,
         UnsignedDivideInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def uremInst {
    test("%x = urem i64 1, 2", Parser.instruction,
         UnsignedRemainderInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def xorInst {
    test("%x = xor i64 1, 2", Parser.instruction,
         XorInstruction("%x", IntType(64), IntValue(1, 64), IntValue(2, 64)))
  }

  @Test
  def zextInst {
    test("%x = zext i32 0 to i64", Parser.instruction,
         ZeroExtendInstruction("%x", IntValue(0, 32), IntType(64)))
  }

  @Test
  def blockTest {
    test("bb0:\n" +
         "  ret i32 0\n",
         Parser.block,
         Block("%bb0", List(ReturnInstruction(IntValue(0L, 32)))))
  }

  @Test
  def defnParameterTest {
    test("i32 inreg %x", Parser.defnParameter,
         Parameter("%x", IntType(32), Set(ParameterAttribute.INREG)))
  }

  @Test
  def functionDefnTest {
    test("define i32 @f(i32 %n) nounwind {\n" +
         "entry:\n" +
         "  ret i32 0\n" +
         "}\n",
         Parser.function,
         Function("@f", Set(), IntType(32), List(Parameter("%n", IntType(32), Set())), 
                  Set(FunctionAttribute.NOUNWIND),
                  List(Block("%entry", List(ReturnInstruction(IntValue(0L, 32)))))))
  }

  @Test
  def simpleModuleTest {
    val targetDatalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128"
    val targetTriple = "x86_64-linux-gnu"
    val program = "; ModuleID = '<stdin>'\n" +
                  "target datalayout = \"" + targetDatalayout + "\"\n" +
                  "target triple = \"" + targetTriple + "\"\n" +
                  "\n" +
                  "define i32 @main() nounwind {\n" +
                  "entry:\n" +
                  "  %\"alloca point\" = bitcast i32 0 to i32          ; <i32> [#uses=0]\n" +
                  "  br label %return\n" +
                  "\n" +
                  "return:                                           ; preds = %entry\n" +
                  "  ret i32 0\n" +
                  "}\n"
    val function = Function("@main", Set(), IntType(32), Nil, Set(FunctionAttribute.NOUNWIND),
                            List(Block("%entry",
                                       List(BitCastInstruction("%alloca point",
                                                               IntValue(0L, 32),
                                                               IntType(32)),
                                            BranchInstruction(DefinedValue("%return",
                                                                           LabelType)))),
                                 Block("%return",
                                       List(ReturnInstruction(IntValue(0L, 32))))))
    val expected = new Module(Some(targetDatalayout), Some(targetTriple),
                              Map((function.name -> function)))
    test(program, Parser.module, expected)
  }
}
