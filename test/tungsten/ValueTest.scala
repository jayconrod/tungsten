package tungsten

import org.junit.Test
import org.junit.Assert._

class ValueTest {
  val loc = Location("foo.w", 1, 2, 3, 4)
  val module = new Module

  @Test
  def unit = {
    assertEquals(UnitType(), UnitValue(loc).ty(module))
  }

  @Test
  def int = {
    assertEquals(IntType(32), Int32Value(12, loc).ty(module))
  }

  @Test
  def global = {
    val foo = new Symbol("foo")
    val global = Global(foo, UnitType(), Some(UnitValue()))
    val module = new Module
    module.add(global)

    assertEquals(PointerType(UnitType()), DefinedValue(foo).ty(module))
  }
}
