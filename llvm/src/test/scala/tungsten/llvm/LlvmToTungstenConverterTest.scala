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
  import converter._

  def testConversion[T <: tungsten.Definition](expected: T, actual: T) {
    assertEquals(expected, actual)
    assertEquals(cDefinitions(expected.name), expected)
  }

  @Test
  def emptyFunction {
    val function = Function("empty", VoidType, Nil, Nil, Nil)
    testConversion(tungsten.Function("empty", Nil, tungsten.UnitType(), Nil),
                   convertFunction(function))
  }

  @Test
  def parameterTest {
    val parameter = Parameter("a", IntType(32), Nil)
    parents ::= "foo"
    testConversion(tungsten.Parameter(new Symbol(List("foo", "a")), tungsten.IntType(32)),
                   convertParameter(parameter))
  }

  @Test
  def allocaInst {
    parents = List("bar", "foo")
    testConversion(tungsten.StackAllocateInstruction("foo.bar.a",
                                                     tungsten.PointerType(tungsten.IntType(32))),
                   convertInstruction(AllocaInstruction("a", IntType(32))))
  }

  @Test
  def bitcastInst {
    parents = List("bar", "foo")
    testConversion(tungsten.AssignInstruction("foo.bar.a",
                                              tungsten.Int32Value(0)),
                   convertInstruction(BitcastInstruction("a", IntValue(0L, 32), IntType(32))))
  }

  // TODO: branchInst

  @Test 
  def loadInst {
    parents = List("bar", "foo")
    testConversion(tungsten.LoadInstruction("foo.bar.a", tungsten.DefinedValue("foo.bar.p")),
                   convertInstruction(LoadInstruction("a",
                                                      DefinedValue("p", PointerType(IntType(32))))))
  }

  @Test
  def retInst {
    parents = List("bar", "foo")
    testConversion(tungsten.ReturnInstruction("foo.bar.anon$#1",
                                              tungsten.Int32Value(12)),
                   convertInstruction(ReturnInstruction(IntValue(12L, 32))))
  }

  @Test
  def storeInst {
    parents = List("bar", "foo")
    testConversion(tungsten.StoreInstruction("foo.bar.anon$#1",
                                             tungsten.DefinedValue("foo.bar.p"),
                                             tungsten.DefinedValue("foo.bar.v")),
                   convertInstruction(StoreInstruction(DefinedValue("v", IntType(32)), 
                                                       DefinedValue("p", PointerType(IntType(32))), 
                                                       4)))
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
    parents ::= "foo"
    assertEquals(tungsten.DefinedValue("foo.a"), convertValue(DefinedValue("a", IntType(32))))
  }
}

class LlvmLivenessAnalysisTest {
  @Test
  def cfgTest {
    val bb1 = Block("bb1", List(BranchInstruction(DefinedValue("bb2", LabelType))))
    val bb2 = Block("bb2", List(BranchInstruction(DefinedValue("bb1", LabelType))))
    val blocks = List(bb1, bb2)
    val f = Function("f", VoidType, Nil, Nil, blocks)
    val analysis = new LlvmLivenessAnalysis(f)

    val nodes = blocks.map(new analysis.Node(_))
    var g = new Graph(nodes) & ((nodes(0), nodes(1))) & ((nodes(1), nodes(0)))

    assertEquals(g, analysis.cfg)
  }
}
