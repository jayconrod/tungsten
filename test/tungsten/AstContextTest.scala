package tungsten

import org.junit.Test
import org.junit.Assert._

class AstContextTest {
  val ctx = new AstContext

  @Test
  def resolveEmpty = {
    val name = new Symbol("foo")
    val global = Global(name, UnitType(), None)
    ctx.module.add(global)
    assertSame(global, ctx.resolve(name).get)
  }

  @Test
  def resolveUsingStack = {
    val name = new Symbol(List("foo", "bar"))
    val global = Global(name, UnitType(), None)
    ctx.module.add(global)
    ctx.names.push(new Symbol("foo"))
    val resolved = ctx.resolve(new Symbol("bar")).get
    assertSame(global, resolved)
  }

  @Test
  def resolveWithoutStack = {
    val name = new Symbol(List("foo"))
    val global = Global(name, UnitType(), None)
    ctx.module.add(global)
    ctx.names.push(new Symbol("bar"))
    assertSame(global, ctx.resolve(name).get)
  }

  @Test
  def resolveShadowed = {
    val foobar = Global(new Symbol(List("foo", "bar")), UnitType(), None)
    ctx.module.add(foobar)
    val bar = Global(new Symbol("bar"), UnitType(), None)
    ctx.module.add(bar)
    ctx.names.push(new Symbol("foo"))
    val resolved = ctx.resolve(new Symbol("bar")).get
    assertSame(foobar, resolved)
  }
}
