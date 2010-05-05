package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverterTest {
  val dummyConverter = new TungstenToLlvmConverter(new tungsten.Module)

  @Test
  def globalSymbol {
    assertEquals("@x", dummyConverter.globalSymbol("x"))
    assertEquals("@x.y", dummyConverter.globalSymbol("x.y"))
    assertEquals("@x.y.1", dummyConverter.globalSymbol("x.y#1"))
    assertEquals("@\"multi word\"", dummyConverter.globalSymbol("\"multi word\""))
    assertEquals("@\"multi\\0aline\"", dummyConverter.globalSymbol("\"multi\\000aline\""))
    assertEquals("@\"\\12\\34\"", dummyConverter.globalSymbol("\"\\1234\""))
  }
}
