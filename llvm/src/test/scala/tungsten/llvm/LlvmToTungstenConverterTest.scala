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
    testConversion(tungsten.Function("empty", Nil, tungsten.UnitType(), Nil),
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
                                              tungsten.Int32Value(0)),
                   convertInstruction(BitcastInstruction("%a", IntValue(0L, 32), IntType(32)),
                                      defaultData))
  }

  @Test
  def branchInst {
    parent = "foo"
    val data = BlockParameterData(Nil, Map(("%baz" -> List(IntValue(0L, 32)))))
    testConversion(tungsten.BranchInstruction("foo.anon$#1",
                                              "foo.baz#2",
                                              List(tungsten.Int32Value(0))),
                   convertInstruction(BranchInstruction(DefinedValue("%baz", LabelType)), data))
  }

  @Test 
  def loadInst {
    parent = "foo"
    testConversion(tungsten.LoadInstruction("foo.a#1", tungsten.DefinedValue("foo.p#2")),
                   convertInstruction(LoadInstruction("%a",
                                                      DefinedValue("%p", PointerType(IntType(32))),
                                                      None),
                                      defaultData))
  }

  @Test
  def retInst {
    parent = "foo"
    testConversion(tungsten.ReturnInstruction("foo.anon$#1",
                                              tungsten.Int32Value(12)),
                   convertInstruction(ReturnInstruction(IntValue(12L, 32)), defaultData))
  }

  @Test
  def storeInst {
    parent = "foo"
    testConversion(tungsten.StoreInstruction("foo.anon$#1",
                                             tungsten.DefinedValue("foo.p#2"),
                                             tungsten.DefinedValue("foo.v#3")),
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
                                                tungsten.DefinedValue("foo.x#2")),
                     convertInstruction(ReturnInstruction(DefinedValue("%x", IntType(32))),
                                        defaultData))
    }
  }

  @Test
  def convertVoidType {
    assertEquals(tungsten.UnitType(), convertType(VoidType))
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
    assertEquals(tungsten.BooleanType(), convertType(IntType(1)))
  }

  @Test
  def convertWeirdIntType {
    assertEquals(tungsten.IntType(32), convertType(IntType(17)))
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
  def convertIntValue {
    assertEquals(tungsten.Int32Value(12), convertValue(IntValue(12L, 32)))
  }

  @Test
  def convertDefinedValue {
    parent = "foo"
    assertEquals(tungsten.DefinedValue("foo.a#1"), convertValue(DefinedValue("%a", IntType(32))))
  }
}

