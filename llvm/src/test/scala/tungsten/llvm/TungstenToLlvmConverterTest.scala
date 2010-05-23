package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverterTest {
  val dummyConverter = new TungstenToLlvmConverter(new tungsten.Module)
  val parent = Symbol("foo")

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
    assertEquals("\"multi\\0aline\"", dummyConverter.convertSymbol("\"multi\\000aline\""))
    assertEquals("\"\\12\\34\"", dummyConverter.convertSymbol("\"\\1234\""))
  }

  @Test
  def globalSymbol {
    assertEquals("@x", dummyConverter.globalSymbol("x"))
  }

  @Test
  def localSymbol {
    assertEquals("%x", dummyConverter.localSymbol("y.x", "y"))
    assertEquals("%x.y", dummyConverter.localSymbol("x.y", "x.y"))
    assertEquals("%x.y", dummyConverter.localSymbol("x.y", "x.y.z"))
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
    assertEquals(ArrayType(0L, IntType(32)),
                 dummyConverter.convertType(tungsten.ArrayType(None, tungsten.IntType(32))))
    assertEquals(ArrayType(2L, IntType(32)),
                 dummyConverter.convertType(tungsten.ArrayType(Some(2L), tungsten.IntType(32))))
    assertEquals(NamedStructType("%A"),
                 dummyConverter.convertType(tungsten.StructType("A")))
  }

  @Test
  def convertValues {
    def convert(value: tungsten.Value) = dummyConverter.convertValue(value, Symbol("dummy"))
    assertEquals(VoidValue, convert(tungsten.UnitValue))
    assertEquals(IntValue(1L, 1), convert(tungsten.BooleanValue(true)))
    assertEquals(IntValue(0L, 1), convert(tungsten.BooleanValue(false)))
    assertEquals(FloatValue(0.0, 32), convert(tungsten.FloatValue(0.0, 32)))
    assertEquals(FloatValue(0.0, 64), convert(tungsten.FloatValue(0.0, 64)))
    assertEquals(NullValue(IntType(8)), convert(tungsten.NullValue))
    assertEquals(ArrayValue(IntType(32), List(IntValue(12L, 32))),
                 convert(tungsten.ArrayValue(tungsten.IntType(32), List(tungsten.IntValue(12L, 32)))))
    assertEquals(StructValue(List(IntValue(12L, 32))),
                 convert(tungsten.StructValue("A", List(tungsten.IntValue(12L, 32)))))
  }

  @Test
  def convertDefinedValue {
    assertEquals(DefinedValue("%x", IntType(32)),
                 dummyConverter.convertValue(tungsten.DefinedValue("foo.x", tungsten.IntType(32)),
                                             Symbol("foo")))
  }

  @Test
  def convertWordValue {
    assertEquals(IntValue(0L, 32), 
                 dummyConverter.convert32BitValue(tungsten.IntValue(0L, 64), parent))
    assertEquals(DefinedValue("%a", IntType(32)),
                 dummyConverter.convert32BitValue(tungsten.DefinedValue("foo.a", tungsten.IntType(64)), parent))
  }

  @Test
  def addressInst {
    val expected = GetElementPointerInstruction("%x",
                                                DefinedValue("%y", PointerType(IntType(8))),
                                                List(IntValue(0L, 32)))
    val address = tungsten.AddressInstruction("foo.x",
                                              tungsten.PointerType(tungsten.IntType(8)),
                                              tungsten.DefinedValue("foo.y", tungsten.PointerType(tungsten.IntType(8))),
                                              List(tungsten.IntValue(0L, 64)))
    testInstructionConversion(expected, address)
  }

  @Test
  def assignInst {
    val expected = BitcastInstruction("%x", IntValue(0L, 32), IntType(32))
    val assign = tungsten.AssignInstruction("foo.x", tungsten.IntType(32), tungsten.IntValue(0L, 32))
    testInstructionConversion(expected, assign)
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
  def fextendInst {
    val expected = FloatExtendInstruction("%x", FloatValue(0.0, 32), FloatType(64))
    val fextend = tungsten.FloatExtendInstruction("foo.x",
                                                  tungsten.FloatType(64),
                                                  tungsten.FloatValue(0.0, 32))
    testInstructionConversion(expected, fextend)
  }

  @Test
  def ftoiInst {
    val expected = FloatToIntegerInstruction("%x", FloatValue(0.0, 32), IntType(32))
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
  def isextend {
    val expected = IntegerSignExtendInstruction("%x", IntValue(12, 32), IntType(64))
    val isextend = tungsten.IntegerSignExtendInstruction("foo.x",
                                                         tungsten.IntType(64),
                                                         tungsten.IntValue(12, 32))
    testInstructionConversion(expected, isextend)
  }

  @Test
  def itofInst {
    val expected = IntegerToFloatInstruction("%x", IntValue(12, 32), FloatType(32))
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
}
