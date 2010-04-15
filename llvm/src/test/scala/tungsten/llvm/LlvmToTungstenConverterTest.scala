package tungsten.llvm

import scala.collection.mutable.{Set => MSet}
import org.junit.Test
import org.junit.Assert._
import tungsten.Symbol
import tungsten.Utilities._

class LlvmToTungstenConverterTest {
  import LlvmToTungstenConverter._

  val cDefinitions = MSet[tungsten.Definition]()

  def testConversion[T <: tungsten.Definition](expected: T, actual: T) {
    assertEquals(expected, actual)
    assertTrue(cDefinitions(expected))
  }

  @Test
  def emptyFunction {
    val function = Function("empty", VoidType, Nil, Nil, Nil)
    testConversion(tungsten.Function("empty", Nil, tungsten.UnitType(), Nil),
                   convertFunction(function, cDefinitions))
  }

  @Test
  def parameterTest {
    val parameter = Parameter("a", IntType(32), Nil)
    val parent = new Symbol("foo")
    testConversion(tungsten.Parameter(new Symbol(List("foo", "a")), tungsten.IntType(32)),
                   convertParameter(parameter, parent, cDefinitions))
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
    assertEquals(tungsten.Int32Value(12),
                 convertValue(IntValue(12L, 32), Some("foo")))
  }

  @Test
  def convertDefinedValue {
    assertEquals(tungsten.DefinedValue("foo.a"),
                 convertValue(DefinedValue("a", IntType(32)), Some("foo")))
  }
}
