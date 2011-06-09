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

class InstructionTest {
  val module = new Module(None, None, Map())

  @Test
  def gepTyBasic {
    val gep = GetElementPointerInstruction("a", DefinedValue("b", PointerType(IntType(32))),
                                           List(IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test
  def gepTyArray {
    val gep = GetElementPointerInstruction("a", 
                                           DefinedValue("b", PointerType(ArrayType(2L, IntType(32)))),
                                           List(IntValue(0L, 32), IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test
  def gepAnonStructTy {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test
  def gepNamedStructTy {
    val struct = Struct("%A", List(IntType(32)))
    val module = new Module(None, None, Map((struct.name -> struct)))
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(NamedStructType("%A"))),
                                           List(IntValue(0L, 32), IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test(expected=classOf[LlvmConversionException])
  def gepStructTyNonConstIndex {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), DefinedValue("c", IntType(32))))
    gep.ty(module)
  }

  @Test(expected=classOf[LlvmConversionException])
  def gepStructTyWeirdSizeIndex {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), IntValue(0L, 31)))
    gep.ty(module)
  }

  @Test(expected=classOf[LlvmConversionException])
  def gepStructTyIndexOutOfBounds {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), IntValue(1L, 32)))
    gep.ty(module)
  }

  @Test
  def addToString {
    assertEquals("%x = add i64 2, 2",
                 AddInstruction("%x", IntType(64), IntValue(2, 64), IntValue(2, 64)).toString)
  }

  @Test
  def andToString {
    assertEquals("%x = and i64 2, 2",
                 AndInstruction("%x", IntType(64), IntValue(2, 64), IntValue(2, 64)).toString)
  }

  @Test
  def asrToString {
    assertEquals("%x = asr i64 2, 2",
                 ArithmeticShiftRightInstruction("%x", IntType(64), IntValue(2, 64), IntValue(2, 64)).toString)
  }

  @Test
  def brToString {
    assertEquals("br label %bb",
                 BranchInstruction(DefinedValue("%bb", LabelType)).toString)
  }

  @Test
  def callToString {
    assertEquals("%x = tail call fastcc zeroext i64 @f(i64 2) nounwind",
                 CallInstruction("%x",
                                 true,
                                 Some("fastcc"),
                                 Set(ParameterAttribute.ZEROEXT),
                                 IntType(64),
                                 None,
                                 DefinedValue("@f", FunctionType(IntType(64), List(IntType(64)))),
                                 List(IntValue(2, 64)),
                                 Set(FunctionAttribute.NOUNWIND)).toString)
  }

  @Test
  def conditionalToString {
    assertEquals("br i1 1, label %bb1, label %bb2",
                 ConditionalBranchInstruction(IntValue(1, 1),
                                              DefinedValue("%bb1", LabelType),
                                              DefinedValue("%bb2", LabelType)).toString)
  }

  @Test
  def fcmpToString {
    assertEquals("%x = fcmp olt double 1.000000e+00, 2.000000e+00",
                 FloatCompareInstruction("%x", Comparison.OLT, FloatType(64), 
                                         FloatValue(1.0, 64), FloatValue(2.0, 64)).toString)
  }

  @Test
  def fpextToString {
    assertEquals("%x = fpext float 1.000000e+00 to double",
                 FloatExtendInstruction("%x", FloatValue(1.0, 32), FloatType(64)).toString)
  }

  @Test
  def getelementptrToString {
    assertEquals("%x = getelementptr i64* %y, i64 1",
                 GetElementPointerInstruction("%x",
                                              DefinedValue("%y", PointerType(IntType(64))),
                                              List(IntValue(1, 64))).toString)
  }

  @Test
  def icmpToString {
    assertEquals("%x = icmp slt i64 1, 2",
                 IntegerCompareInstruction("%x",
                                           Comparison.SLT,
                                           IntType(64),
                                           IntValue(1, 64), IntValue(2, 64)).toString)
  }

  @Test
  def loadToString {
    assertEquals("%x = load i64* %p, align 4",
                 LoadInstruction("%x", DefinedValue("%p", PointerType(IntType(64))), Some(4)).toString)
  }

  @Test
  def phiToString {
    assertEquals("%x = phi i64 [1, %bb1], [2, %bb2]",
                 PhiInstruction("%x", 
                                IntType(64), 
                                List((IntValue(1, 64), "%bb1"), (IntValue(2, 64), "%bb2"))).toString)
  }

  @Test
  def retToString {
    assertEquals("ret i64 1",
                 ReturnInstruction(IntValue(1, 64)).toString)
  }

  @Test
  def storeInst {
    assertEquals("store i64 12, i64* %p, align 4",
                 StoreInstruction(IntValue(12, 64), 
                                  DefinedValue("%p", PointerType(IntType(64))), 
                                  Some(4)).toString)
  }
}
