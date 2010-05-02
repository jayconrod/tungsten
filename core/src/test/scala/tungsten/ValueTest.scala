package tungsten

import org.junit.Test
import org.junit.Assert._

class ValueTest {
  var module = new Module

  @Test
  def unit {
    assertEquals(UnitType, UnitValue.ty(module))
  }

  @Test
  def int {
    assertEquals(IntType(32), IntValue(12, 32).ty(module))
  }

  @Test
  def global {
    val foo = Symbol("foo")
    val global = Global(foo, UnitType, Some(UnitValue))
    module = module.add(global)

    assertEquals(PointerType(UnitType), DefinedValue(foo).ty(module))
  }
}
