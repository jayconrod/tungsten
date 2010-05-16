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
                                                 tungsten.IntType(32), 
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
}
