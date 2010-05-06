package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverterTest {
  val dummyConverter = new TungstenToLlvmConverter(new tungsten.Module)

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
    assertEquals(FloatType, dummyConverter.convertType(tungsten.FloatType(32)))
    assertEquals(DoubleType, dummyConverter.convertType(tungsten.FloatType(64)))
    assertEquals(PointerType(IntType(32)), 
                 dummyConverter.convertType(tungsten.PointerType(tungsten.IntType(32))))
    assertEquals(PointerType(IntType(8)), dummyConverter.convertType(tungsten.NullType))
    assertEquals(PointerType(IntType(8)), 
                 dummyConverter.convertType(tungsten.PointerType(tungsten.UnitType)))
    assertEquals(ArrayType(0L, IntType(32)),
                 dummyConverter.convertType(tungsten.ArrayType(None, tungsten.IntType(32))))
    assertEquals(ArrayType(2L, IntType(32)),
                 dummyConverter.convertType(tungsten.ArrayType(Some(2L), tungsten.IntType(32))))
  }
}
