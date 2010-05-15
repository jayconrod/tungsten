package tungsten.llvm

import scala.collection.mutable.{Set => MSet}
import org.junit.Test
import org.junit.Assert._
import tungsten.Symbol
import tungsten.Graph
import tungsten.Utilities._

class LlvmToTungstenConverterTest {
  var module = new Module(None, None, Map[String, Definition]())
  val converter = new LlvmToTungstenConverter(module)
  val defaultData = BlockParameterData(Nil, Map())
  import converter._

  def testConversion[T <: tungsten.Definition](expected: T, actual: T) {
    assertEquals(expected, actual)
    assertEquals(cDefinitions(expected.name), expected)
  }

  @Test
  def emptyFunction {
    val function = Function("@empty", VoidType, Nil, Nil, Nil)
    testConversion(tungsten.Function("empty", tungsten.UnitType, Nil, Nil),
                   convertFunction(function))
  }

  @Test
  def parameterTest {
    val parameter = Parameter("%a", IntType(32), Nil)
    parent = "foo"
    testConversion(tungsten.Parameter("foo.a#1", tungsten.IntType(32)),
                   convertParameter(parameter))
  }

  @Test
  def allocaInst {
    parent = "foo"
    testConversion(tungsten.StackAllocateInstruction("foo.a#1",
                                                     tungsten.PointerType(tungsten.IntType(32))),
                   convertInstruction(AllocaInstruction("%a", IntType(32)), defaultData))
  }

  @Test
  def bitcastInst {
    parent = "foo"
    testConversion(tungsten.AssignInstruction("foo.a#1",
                                              tungsten.IntType(32),
                                              tungsten.IntValue(0L, 32)),
                   convertInstruction(BitcastInstruction("%a", IntValue(0L, 32), IntType(32)),
                                      defaultData))
  }

  @Test
  def branchInst {
    parent = "foo"
    val data = BlockParameterData(Nil, Map(("%baz" -> List(IntValue(0L, 32)))))
    testConversion(tungsten.BranchInstruction("foo.anon$#1",
                                              tungsten.UnitType,
                                              "foo.baz#2",
                                              List(tungsten.IntValue(0L, 32))),
                   convertInstruction(BranchInstruction(DefinedValue("%baz", LabelType)), data))
  }

  @Test 
  def loadInst {
    parent = "foo"
    testConversion(tungsten.LoadInstruction("foo.a#1", 
                                            tungsten.IntType(32),
                                            tungsten.DefinedValue("foo.p#2", tungsten.PointerType(tungsten.IntType(32)))),
                   convertInstruction(LoadInstruction("%a",
                                                      DefinedValue("%p", PointerType(IntType(32))),
                                                      None),
                                      defaultData))
  }

  @Test
  def retInst {
    parent = "foo"
    testConversion(tungsten.ReturnInstruction("foo.anon$#1",
                                              tungsten.UnitType,
                                              tungsten.IntValue(12L, 32)),
                   convertInstruction(ReturnInstruction(IntValue(12L, 32)), defaultData))
  }

  @Test
  def storeInst {
    parent = "foo"
    testConversion(tungsten.StoreInstruction("foo.anon$#1",
                                             tungsten.UnitType,
                                             tungsten.DefinedValue("foo.v#2", tungsten.IntType(32)),
                                             tungsten.DefinedValue("foo.p#3", tungsten.PointerType(tungsten.IntType(32)))),
                   convertInstruction(StoreInstruction(DefinedValue("%v", IntType(32)), 
                                                       DefinedValue("%p", PointerType(IntType(32))), 
                                                       Some(4)),
                                      defaultData))
  }

  @Test
  def repeatName {
    parent = "foo"
    for (i <- List(1, 3)) {
      val retSymbol = Symbol(List("foo", "anon$"), i)
      testConversion(tungsten.ReturnInstruction(retSymbol,
                                                tungsten.UnitType,
                                                tungsten.DefinedValue("foo.x#2", tungsten.IntType(32))),
                     convertInstruction(ReturnInstruction(DefinedValue("%x", IntType(32))),
                                        defaultData))
    }
  }

  @Test
  def convertVoidType {
    assertEquals(tungsten.UnitType, convertType(VoidType))
  }

  @Test
  def convertIntType {
    assertEquals(tungsten.IntType(8), convertType(IntType(8)))
    assertEquals(tungsten.IntType(16), convertType(IntType(16)))
    assertEquals(tungsten.IntType(32), convertType(IntType(32)))
    assertEquals(tungsten.IntType(64), convertType(IntType(64)))
  }

  @Test
  def convertBooleanType {
    assertEquals(tungsten.BooleanType, convertType(IntType(1)))
  }

  @Test
  def convertWeirdIntType {
    assertEquals(tungsten.IntType(32), convertType(IntType(17)))
  }

  @Test
  def convertFloatTypes {
    assertEquals(tungsten.FloatType(32), convertType(FloatType(32)))
    assertEquals(tungsten.FloatType(64), convertType(FloatType(64)))
  }

  @Test(expected=classOf[UnsupportedOperationException])
  def convertLabelType {
    convertType(LabelType)
  }

  @Test
  def convertPointerType {
    assertEquals(tungsten.PointerType(tungsten.IntType(32)),
                 convertType(PointerType(IntType(32))))
  }

  @Test
  def convertArrayType {
    assertEquals(tungsten.ArrayType(None, tungsten.IntType(32)),
                 convertType(ArrayType(0L, IntType(32))))
    assertEquals(tungsten.ArrayType(Some(3), tungsten.IntType(32)),
                 convertType(ArrayType(3L, IntType(32))))
  }

  @Test
  def convertIntValue {
    assertEquals(tungsten.IntValue(12L, 32), convertValue(IntValue(12L, 32)))
  }

  @Test
  def convertDefinedValue {
    parent = "foo"
    assertEquals(tungsten.DefinedValue("foo.a#1", tungsten.IntType(32)), 
                 convertValue(DefinedValue("%a", IntType(32))))
  }
}

