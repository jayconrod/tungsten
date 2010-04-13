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
    ctx.names.push("foo")
    ctx.addDefn(Global("foo.bar", UnitType(), None))
    ctx.addDefn(Global("bar", UnitType(), None))
    val resolved = ctx.resolve("bar").get
    assertEquals(symbolFromString("bar"), resolved)
  }

  @Test
  def createSimpleName {
    val inst = AstAssignInstruction("b", AstUnitValue(Nowhere), Nowhere)
    ctx.names.push("a")
    val cInst = inst.compile(ctx)
    assertEquals(symbolFromString("a.b"), cInst.name)
  }

  @Test
  def createComplexName {
    val inst = AstAssignInstruction("a.b", AstUnitValue(Nowhere), Nowhere)
    ctx.names.push("foo")
    val cInst = inst.compile(ctx)
    assertEquals(inst.name, cInst.name)
  }

  @Test
  def resolveSimpleGlobalName {
    ctx.names.push("a")
    val name = symbolFromString("b")
    ctx.addDefn(Global(name, UnitType(), None))
    ctx.addDefn(Global("a.b", UnitType(), None))
    val resolved = ctx.resolve("b")
    assertEquals(Some(name), resolved)
  }

  @Test
  def resolveComplexName {
    ctx.names.push("a")
    val name = symbolFromString("x.y")
    ctx.addDefn(Global(name, UnitType(), None))
    ctx.addDefn(Global("a.x.y", UnitType(), None))
    val resolved = ctx.resolve("x.y")
    assertEquals(Some(name), resolved)
  }
}
