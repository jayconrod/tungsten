package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class AstContextTest {
  val ctx = new AstContext

  @Test
  def resolveEmpty = {
    val name = symbolFromString("foo")
    ctx.addDefn(Global(name, UnitType(), None))
    val resolved = ctx.resolve(name).get
    assertEquals(name, resolved)
  }

  @Test
  def resolveUsingStack = {
    val name = symbolFromString("foo.bar")
    val global = Global(name, UnitType(), None)
    ctx.addDefn(global)
    ctx.names.push("foo")
    val resolved = ctx.resolve("bar").get
    assertEquals(name, resolved)
  }

  @Test
  def resolveWithoutStack = {
    val foo = symbolFromString("foo")
    ctx.addDefn(Global(foo, UnitType(), None))
    ctx.names.push("bar")
    val resolved = ctx.resolve("foo").get
    assertEquals(foo, resolved)
  }

  @Test
  def resolveShadowed = {
    val foobar = symbolFromString("foo.bar")
    ctx.addDefn(Global(foobar, UnitType(), None))
    ctx.addDefn(Global("bar", UnitType(), None))
    ctx.names.push("foo")
    val resolved = ctx.resolve("bar").get
    assertEquals(foobar, resolved)
  }
}
