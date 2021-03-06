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

import scala.collection.immutable.TreeMap
import org.junit.Test
import org.junit.Assert._
import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverterTest {
  val dummyConverter = new TungstenToLlvmConverter(new tungsten.Module)
  val parent = Some(Symbol("foo"))

  def testTypeConversion(expected: Type, given: tungsten.Type) {
    val converted = dummyConverter.convertType(given)
    assertEquals(expected, converted)
  }

  def testInstructionConversion(expected: Instruction, given: tungsten.Instruction) {
    val converted = dummyConverter.convertInstruction(given, parent)
    assertEquals(expected, converted)
  }

  @Test
  def convertSymbol {
    assertEquals("x", dummyConverter.convertSymbol("x"))
    assertEquals("x.y", dummyConverter.convertSymbol("x.y"))
    assertEquals("x.y.1", dummyConverter.convertSymbol("x.y#1"))
    assertEquals("\"multi word\"", dummyConverter.convertSymbol("\"multi word\""))
    assertEquals("\"multi\\0aline\"", dummyConverter.convertSymbol("\"multi\\0aline\""))
    assertEquals("\"\\124\"", dummyConverter.convertSymbol("\"\\12\\34\""))
  }

  @Test
  def globalSymbol {
    assertEquals("@x", dummyConverter.globalSymbol("x"))
  }

  @Test
  def structSymbol {
    assertEquals("%T", dummyConverter.structSymbol("T"))
  }

  @Test
  def localSymbol {
    assertEquals("%x", dummyConverter.localSymbol("y.x", Some("y")))
    assertEquals("%x.y", dummyConverter.localSymbol("x.y", Some("x.y")))
    assertEquals("%x.y", dummyConverter.localSymbol("x.y", Some("x.y.z")))
  }

  @Test
  def convertTypes {
    assertEquals(VoidType, dummyConverter.convertType(tungsten.UnitType))
    assertEquals(IntType(1), dummyConverter.convertType(tungsten.BooleanType))
    assertEquals(IntType(32), dummyConverter.convertType(tungsten.IntType(32)))
    assertEquals(FloatType(32), dummyConverter.convertType(tungsten.FloatType(32)))
    assertEquals(FloatType(64), dummyConverter.convertType(tungsten.FloatType(64)))
    assertEquals(PointerType(IntType(32)), 
                 dummyConverter.convertType(tungsten.PointerType(tungsten.IntType(32))))
    assertEquals(PointerType(IntType(8)), dummyConverter.convertType(tungsten.NullType))
    assertEquals(PointerType(IntType(8)), 
                 dummyConverter.convertType(tungsten.PointerType(tungsten.UnitType)))
    assertEquals(ArrayType(2L, IntType(32)),
                 dummyConverter.convertType(tungsten.ArrayType(2L, tungsten.IntType(32))))
    assertEquals(NamedStructType("%A"),
                 dummyConverter.convertType(tungsten.StructType("A")))
  }

  @Test
  def convertValues {
    def convert(value: tungsten.Value) = dummyConverter.convertValue(value, parent)
    assertEquals(VoidValue, convert(tungsten.UnitValue))
    assertEquals(IntValue(1L, 1), convert(tungsten.BooleanValue(true)))
    assertEquals(IntValue(0L, 1), convert(tungsten.BooleanValue(false)))
    assertEquals(FloatValue(0.0, 32), convert(tungsten.FloatValue(0.0, 32)))
    assertEquals(FloatValue(0.0, 64), convert(tungsten.FloatValue(0.0, 64)))
    assertEquals(NullValue(IntType(8)), convert(tungsten.NullValue))
    assertEquals(ArrayValue(IntType(32), List(IntValue(12L, 32))),
                 convert(tungsten.ArrayValue(tungsten.IntType(32), List(tungsten.IntValue(12L, 32)))))
    assertEquals(NamedStructValue("%A", List(IntValue(12L, 32))),
                 convert(tungsten.StructValue("A", List(tungsten.IntValue(12L, 32)))))
  }

  @Test
  def convertDefinedValue {
    assertEquals(DefinedValue("%x", IntType(32)),
                 dummyConverter.convertValue(tungsten.DefinedValue("foo.x", tungsten.IntType(32)),
                                             parent))
  }

  @Test
  def convertSpecialValue {
    val value = tungsten.BitCastValue(tungsten.DefinedValue("foo.x", tungsten.PointerType(tungsten.IntType(64))),
                                      tungsten.PointerType(tungsten.IntType(8)))
    val expected = BitCastValue(DefinedValue("%x", PointerType(IntType(64))), 
                                PointerType(IntType(8)))
    assertEquals(expected, dummyConverter.convertValue(value, parent))
  }                 

  @Test
  def convertGlobalDefinedValue {
    val global = tungsten.Global("g", tungsten.IntType(64), None)
    val definitions = TreeMap(global.name -> global)
    val module = new tungsten.Module(definitions=definitions)
    val expected = DefinedValue("@g", PointerType(IntType(64)))
    val value = tungsten.DefinedValue("g", tungsten.PointerType(tungsten.IntType(64)))
    val converter = new TungstenToLlvmConverter(module)
    assertEquals(expected, converter.convertValue(value, parent))
  }    

  @Test
  def addressInst {
    val expected = GetElementPointerInstruction("%x",
                                                DefinedValue("%y", PointerType(IntType(8))),
                                                List(IntValue(0L, 32)))
    val address = tungsten.AddressInstruction("foo.x",
                                              tungsten.PointerType(tungsten.IntType(8)),
                                              tungsten.DefinedValue("foo.y", tungsten.PointerType(tungsten.IntType(8))),
                                              List(tungsten.IntValue(0L, 32)))
    testInstructionConversion(expected, address)
  }

  @Test
  def binopInst {
    val expected = SubtractInstruction("%x", IntType(32), IntValue(2, 32), IntValue(1, 32))
    val sub = tungsten.BinaryOperatorInstruction("foo.x", 
                                                 tungsten.BooleanType,
                                                 tungsten.BinaryOperator.SUBTRACT, 
                                                 tungsten.IntValue(2, 32), 
                                                 tungsten.IntValue(1, 32))
    testInstructionConversion(expected, sub)
  }

  @Test
  def bitcastInst {
    val expected = BitCastInstruction("%x", IntValue(0, 32), IntType(32))
    val bitcast = tungsten.BitCastInstruction("foo.x",
                                              tungsten.IntType(32),
                                              tungsten.IntValue(0, 32))
    testInstructionConversion(expected, bitcast)
  }

  @Test
  def branchInst {
    val expected = BranchInstruction(DefinedValue("%target", LabelType))
    val branch = tungsten.BranchInstruction("foo.x", tungsten.UnitType, "foo.target", Nil)
    testInstructionConversion(expected, branch)
  }

  @Test
  def condInst {
    val expected = ConditionalBranchInstruction(IntValue(1L, 1),
                                                DefinedValue("%t", LabelType),
                                                DefinedValue("%f", LabelType))
    val cond = tungsten.ConditionalBranchInstruction("foo.x",
                                                     tungsten.UnitType,
                                                     tungsten.BooleanValue(true),
                                                     "foo.t",
                                                     Nil,
                                                     "foo.f",
                                                     Nil)
    testInstructionConversion(expected, cond)
  }

  @Test
  def extractInst {
    val expected = ExtractValueInstruction("%x", ArrayValue(IntType(32), List(IntValue(1, 32))),
                                           List(IntValue(0, 32)))
    val extract = tungsten.ExtractInstruction("foo.x", tungsten.IntType(32),
                                              tungsten.ArrayValue(tungsten.IntType(32),
                                                                  List(tungsten.IntValue(1, 32))),
                                              List(tungsten.IntValue(0, 32)))
    testInstructionConversion(expected, extract)
  }

  @Test
  def fextendInst {
    val expected = FloatExtendInstruction("%x", FloatValue(0.0, 32), FloatType(64))
    val fextend = tungsten.FloatExtendInstruction("foo.x",
                                                  tungsten.FloatType(64),
                                                  tungsten.FloatValue(0.0, 32))
    testInstructionConversion(expected, fextend)
  }

  @Test
  def ftoiInst {
    val expected = FloatToSignedIntegerInstruction("%x", FloatValue(0.0, 32), IntType(32))
    val ftoi = tungsten.FloatToIntegerInstruction("foo.x",
                                                  tungsten.IntType(32),
                                                  tungsten.FloatValue(0.0, 32))
    testInstructionConversion(expected, ftoi)
  }

  @Test
  def ftruncateInst {
    val expected = FloatTruncateInstruction("%x", FloatValue(0.0, 64), FloatType(32))
    val ftruncate = tungsten.FloatTruncateInstruction("foo.x",
                                                      tungsten.FloatType(32),
                                                      tungsten.FloatValue(0.0, 64))
    testInstructionConversion(expected, ftruncate)
  }

  @Test
  def insertInst {
    val expected = InsertValueInstruction("%x", ArrayValue(IntType(32), List(IntValue(2, 32))),
                                          IntValue(1, 32),
                                          List(IntValue(0, 32)))
    val insert = tungsten.InsertInstruction("foo.x", tungsten.ArrayType(1, tungsten.IntType(32)),
                                            tungsten.IntValue(1, 32),
                                            tungsten.ArrayValue(tungsten.IntType(32),
                                                                List(tungsten.IntValue(2, 32))),
                                            List(tungsten.IntValue(0, 32)))
    testInstructionConversion(expected, insert)
  }

  @Test
  def isextend {
    val expected = IntegerSignExtendInstruction("%x", IntValue(12, 32), IntType(64))
    val isextend = tungsten.IntegerSignExtendInstruction("foo.x",
                                                         tungsten.IntType(64),
                                                         tungsten.IntValue(12, 32))
    testInstructionConversion(expected, isextend)
  }

  @Test
  def itofInst {
    val expected = SignedIntegerToFloatInstruction("%x", IntValue(12, 32), FloatType(32))
    val itof = tungsten.IntegerToFloatInstruction("foo.x",
                                                  tungsten.FloatType(32),
                                                  tungsten.IntValue(12, 32))
    testInstructionConversion(expected, itof)
  }

  @Test
  def itruncateInst {
    val expected = IntegerTruncateInstruction("%x", IntValue(12, 64), IntType(32))
    val itruncate = tungsten.IntegerTruncateInstruction("foo.x",
                                                        tungsten.IntType(32),
                                                        tungsten.IntValue(12, 64))
    testInstructionConversion(expected, itruncate)
  }

  @Test
  def izextendInst {
    val expected = IntegerZeroExtendInstruction("%x", IntValue(12, 32), IntType(64))
    val izextend = tungsten.IntegerZeroExtendInstruction("foo.x",
                                                         tungsten.IntType(64),
                                                         tungsten.IntValue(12, 32))
    testInstructionConversion(expected, izextend)
  }

  @Test
  def loadInst {
    val expected = LoadInstruction("%x", DefinedValue("%y", PointerType(IntType(32))), None)
    val load = tungsten.LoadInstruction("foo.x",
                                        tungsten.IntType(32),
                                        tungsten.DefinedValue("foo.y", tungsten.PointerType(tungsten.IntType(32))))
    testInstructionConversion(expected, load)
  }

  @Test
  def irelopInst {
    val expected = IntegerCompareInstruction("%x", 
                                             Comparison.SGT,
                                             IntType(32), 
                                             IntValue(12, 32),
                                             IntValue(34, 32))
    val relop = tungsten.RelationalOperatorInstruction("foo.x",
                                                       tungsten.BooleanType,
                                                       tungsten.RelationalOperator.GREATER_THAN,
                                                       tungsten.IntValue(12, 32),
                                                       tungsten.IntValue(34, 32))
    testInstructionConversion(expected, relop)
  }

  @Test
  def prelopInst {
    val expected = IntegerCompareInstruction("%x",
                                             Comparison.EQ,
                                             PointerType(IntType(32)),
                                             DefinedValue("%y", PointerType(IntType(32))),
                                             DefinedValue("%z", PointerType(IntType(32))))
    val relop = tungsten.RelationalOperatorInstruction("foo.x",
                                                       tungsten.BooleanType,
                                                       tungsten.RelationalOperator.EQUAL,
                                                       tungsten.DefinedValue("foo.y", tungsten.PointerType(tungsten.IntType(32))),
                                                       tungsten.DefinedValue("foo.z", tungsten.PointerType(tungsten.IntType(32))))
    testInstructionConversion(expected, relop)
  }

  @Test
  def frelopInst {
    val expected = FloatCompareInstruction("%x",
                                           Comparison.OEQ,
                                           FloatType(64),
                                           FloatValue(1.0, 64),
                                           FloatValue(2.0, 64))
    val relop = tungsten.RelationalOperatorInstruction("foo.x",
                                                       tungsten.BooleanType,
                                                       tungsten.RelationalOperator.EQUAL,
                                                       tungsten.FloatValue(1.0, 64),
                                                       tungsten.FloatValue(2.0, 64))
    testInstructionConversion(expected, relop)
  }

  @Test
  def returnInst {
    val expected = ReturnInstruction(IntValue(12, 64))
    val ret = tungsten.ReturnInstruction("foo.x", tungsten.UnitType, tungsten.IntValue(12, 64))
    testInstructionConversion(expected, ret)
  }

  @Test
  def storeInst {
    val expected = StoreInstruction(IntValue(12, 64), 
                                    DefinedValue("%y", PointerType(IntType(64))),
                                    None)
    val store = tungsten.StoreInstruction("foo.x",
                                          tungsten.UnitType,
                                          tungsten.IntValue(12, 64),
                                          tungsten.DefinedValue("foo.y", tungsten.PointerType(tungsten.IntType(64))))
    testInstructionConversion(expected, store)
  }

  @Test
  def scallInst {
    val param = tungsten.Parameter("f.p", tungsten.IntType(64))
    val function = tungsten.Function("f", tungsten.IntType(64), Nil, List(param.name), Nil)
    val scall = tungsten.StaticCallInstruction("foo.x",
                                               tungsten.IntType(64),
                                               function.name,
                                               Nil,
                                               List(tungsten.IntValue(12, 64)))
    val definitions = TreeMap(param.name -> param,
                              function.name -> function,
                              scall.name -> scall)
    val module = new tungsten.Module(definitions=definitions)
    val converter = new TungstenToLlvmConverter(module)
    val expected = CallInstruction("%x", false, None, Set(), 
                                   IntType(64), None,
                                   DefinedValue("@f", FunctionType(IntType(64), List(IntType(64)))),
                                   List(IntValue(12, 64)),
                                   Set())
    assertEquals(expected, converter.convertInstruction(scall, parent))    
  }

  @Test
  def scallInstNoReturn {
    val noReturn = tungsten.Annotation("tungsten.NoReturn", Nil)
    val function = tungsten.Function("f", tungsten.UnitType, Nil, Nil, Nil,
                                     List(tungsten.AnnotationValue(noReturn.name, Nil)))
    val call = tungsten.StaticCallInstruction("foo.x", tungsten.UnitType, function.name, Nil, Nil)
    val definitions = TreeMap(noReturn.name -> noReturn,
                              function.name -> function,
                              call.name -> call)
    val module = new tungsten.Module(definitions=definitions)
    val converter = new TungstenToLlvmConverter(module)
    val expected = CallInstruction("%x", false, None, Set(),
                                   VoidType, None,
                                   DefinedValue("@f", FunctionType(VoidType, Nil)),
                                   Nil,
                                   Set(FunctionAttribute.NORETURN))
    assertEquals(expected, converter.convertInstruction(call, parent))
  }

  @Test
  def stackInst {
    val expected = AllocaInstruction("%x", IntType(64))
    val stack = tungsten.StackAllocateInstruction("foo.x",
                                                  tungsten.PointerType(tungsten.IntType(64)))
    testInstructionConversion(expected, stack)
  }

  @Test
  def stackArrayInst {
    val expected = AllocaArrayInstruction("%x", IntType(64), IntValue(2, 32))
    val stack = tungsten.StackAllocateArrayInstruction("foo.x",
                                                       tungsten.PointerType(tungsten.IntType(64)),
                                                       tungsten.IntValue(2, 32))
    testInstructionConversion(expected, stack)
  }

  @Test
  def upcastInst {
    val expected = BitCastInstruction("%x", NullValue(IntType(8)), PointerType(IntType(64)))
    val upcast = tungsten.UpcastInstruction("foo.x",
                                            tungsten.PointerType(tungsten.IntType(64)),
                                            tungsten.NullValue)
    testInstructionConversion(expected, upcast)
  }

  @Test
  def block {
    val expected = Block("%bb", List(ReturnInstruction(IntValue(12, 64))))
    val retInst = tungsten.ReturnInstruction("foo.x", tungsten.UnitType, tungsten.IntValue(12, 64))
    val block = tungsten.Block("foo.bb", Nil, List("foo.x"))
    val definitions = TreeMap(block.name -> block, retInst.name -> retInst)
    val module = new tungsten.Module(definitions=definitions)
    val converter = new TungstenToLlvmConverter(module)
    assertEquals(expected, converter.convertBlock(block, parent))
  }

  @Test
  def parameter {
    val expected = Parameter("%x", IntType(64), Set())
    val parameter = tungsten.Parameter("foo.x", tungsten.IntType(64))
    assertEquals(expected, dummyConverter.convertParameter(parameter, parent))
  }

  @Test
  def emptyFunction {
    val expected = Function("@f", Set(), VoidType, Nil, false, Set(), Nil)
    val function = tungsten.Function("f", tungsten.UnitType, Nil, Nil, Nil)
    assertEquals(expected, dummyConverter.convertFunction(function))
  }

  @Test
  def function {
    val expected = Function("@f", Set(), IntType(64),
                            List(Parameter("%x", IntType(64), Set())), false,
                            Set(),
                            List(Block("%entry", List(ReturnInstruction(DefinedValue("%x", IntType(64)))))))
    val parameter = tungsten.Parameter("f.x", tungsten.IntType(64))
    val instruction = tungsten.ReturnInstruction("f.ret", tungsten.UnitType, tungsten.DefinedValue("f.x", tungsten.IntType(64)))
    val block = tungsten.Block("f.entry", Nil, List(instruction.name))
    val function = tungsten.Function("f", tungsten.IntType(64), Nil, List(parameter.name), List(block.name))
    val definitions = TreeMap(parameter.name -> parameter,
                              instruction.name -> instruction,
                              block.name -> block,
                              function.name -> function)
    val module = new tungsten.Module(definitions=definitions)
    val converter = new TungstenToLlvmConverter(module)
    assertEquals(expected, converter.convertFunction(function))
  }

  @Test
  def functionNoReturn {
    val expected = Function("@f", Set(), VoidType, Nil, false, Set(FunctionAttribute.NORETURN), Nil)
    val function = tungsten.Function("f", tungsten.UnitType, Nil, Nil, Nil,
                                     List(tungsten.AnnotationValue("tungsten.NoReturn", Nil)))
    val definitions = TreeMap(function.name -> function)
    val module = new tungsten.Module(definitions=definitions)
    val converter = new TungstenToLlvmConverter(module)
    assertEquals(expected, converter.convertFunction(function))
  }

  @Test
  def struct {
    val expected = Struct("%T", List(IntType(64)))
    val field = tungsten.Field("x", tungsten.IntType(64))
    val struct = tungsten.Struct("T", List(field.name))
    val definitions = TreeMap(field.name -> field, struct.name -> struct)
    val module = new tungsten.Module(definitions=definitions)
    val converter = new TungstenToLlvmConverter(module)
    assertEquals(expected, converter.convertStruct(struct))
  }

  @Test
  def global {
    val expected = Global("@g", Left(IntValue(12, 64)))
    val global = tungsten.Global("g", tungsten.IntType(64), Some(tungsten.IntValue(12, 64)))
    assertEquals(expected, dummyConverter.convertGlobal(global))
  }

  @Test
  def emptyGlobal {
    val expected = Global("@g", Right(IntType(64)))
    val global = tungsten.Global("g", tungsten.IntType(64), None)
    assertEquals(expected, dummyConverter.convertGlobal(global))
  }

  @Test
  def dataLayout {
    val expected = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
    val module = new tungsten.Module(is64Bit=true)
    val converted = TungstenToLlvmConverter(module)
    assertEquals(expected, converted.targetDataLayout.get)
  }
}
