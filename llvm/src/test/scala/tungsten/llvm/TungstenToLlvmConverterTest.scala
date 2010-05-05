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
}
