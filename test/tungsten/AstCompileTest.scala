package tungsten

import org.junit.Test
import org.junit.Assert._

class AstCompileTest {
  val ctx = new AstContext
  val loc = new Location("foo.w", 1, 2, 3, 4)
  val foo = new Symbol("foo")
  val bar = new Symbol("bar")
  val baz = new Symbol("baz")
  val quux = new Symbol("quux")

  def get(name: Symbol) = {
    ctx.module.getDefn(name).get
  }

  def testDefinition(expected: Definition, ast: AstDefinition) = {
    val defn = ast.compile(ctx)
    assertEquals(expected, defn)
    assertEquals(expected, get(expected.name))
    assertTrue(ctx.errors.isEmpty)
  }

  @Test
  def unitType = {
    assertEquals(UnitType(loc), AstUnitType(loc).compile(ctx))
  }

  @Test
  def intType = {
    assertEquals(IntType(32, loc), AstIntType(32, loc).compile(ctx))
  }

  @Test
  def classType = {
    val clas = new Class(foo, Nil, None, Nil, Nil, Nil, Nowhere)
    ctx.module.add(clas)
    assertEquals(ClassType(foo, Nil, loc), AstClassType(foo, Nil, loc).compile(ctx))
  }

  @Test
  def interfaceType = {
    val iface = new Interface(foo, Nil, None, Nil, Nil, Nowhere)
    ctx.module.add(iface)
    assertEquals(InterfaceType(foo, Nil, loc), AstClassType(foo, Nil, loc).compile(ctx))
  }

  @Test
  def defaultType = {
    val ast = AstClassType(foo, Nil, loc)
    val ty = ast.compileOrElse(ctx)
    assertFalse(ctx.errors.isEmpty)
    assertEquals(loc, ty.location)
  }

  @Test
  def unitValue = {
    assertEquals(UnitValue(loc), AstUnitValue(loc).compile(ctx))
  }

  @Test
  def intValues = {
    assertEquals(Int8Value(12, loc), AstInt8Value(12, loc).compile(ctx))
    assertEquals(Int16Value(12, loc), AstInt16Value(12, loc).compile(ctx))
    assertEquals(Int32Value(12, loc), AstInt32Value(12, loc).compile(ctx))
    assertEquals(Int64Value(12, loc), AstInt64Value(12, loc).compile(ctx))
  }

  @Test
  def symbolValue = {
    ctx.names.push(foo)
    val param = Parameter(foo + bar, UnitType(Nowhere))
    ctx.module.add(param)
    val ast = AstSymbolValue(bar, loc)
    val expected = DefinedValue(param.name, loc)
    assertEquals(expected, ast.compile(ctx))
  }

  @Test
  def globalDefn = {
    val global = new Global(foo, UnitType(Nowhere), Some(UnitValue(Nowhere)), loc)
    val ast = AstGlobal(foo, AstUnitType(Nowhere), Some(AstUnitValue(Nowhere)), loc)
    ast.compile(ctx)
    assertEquals(global, get(foo))
  }

  @Test
  def parameter = {
    ctx.names.push(foo)
    val expected = Parameter(foo + bar, UnitType(Nowhere), loc)
    val ast = AstParameter(bar, AstUnitType(Nowhere), loc)
    testDefinition(expected, ast)
  }

  @Test
  def typeParameter = {
    ctx.names.push(foo)
    val expected = TypeParameter(foo + bar, 
                                 Some(UnitType(Nowhere)),
                                 Some(UnitType(Nowhere)),
                                 loc)
    val ast = AstTypeParameter(bar, Some(AstUnitType(Nowhere)), Some(AstUnitType(Nowhere)), loc)
    testDefinition(expected, ast)
  }

  @Test
  def assignInst = {
    ctx.names.push(foo)
    val ast = AstAssignInstruction(bar, AstUnitValue(Nowhere), loc)
    val expected = AssignInstruction(foo + bar, UnitValue(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def branchInst = {
    ctx.names.push(foo)
    val block = Block(foo + baz, Nil, Nil, Nowhere)
    ctx.module.add(block)
    val ast = AstBranchInstruction(bar, baz, List(AstInt32Value(12, Nowhere)), loc)
    val expected = BranchInstruction(foo + bar, 
                                     block.name,
                                     List(Int32Value(12, Nowhere)),
                                     loc)
    testDefinition(expected, ast)
  }

  @Test
  def returnInst = {
    ctx.names.push(foo)
    val ast = AstReturnInstruction(bar, AstUnitValue(Nowhere), loc)
    val expected = ReturnInstruction(foo + bar, UnitValue(Nowhere), loc)
    testDefinition(expected, ast)
  }

  @Test
  def block = {
    ctx.names.push(foo)
    val ast = AstBlock(bar,
                       List(AstParameter(baz, AstUnitType(Nowhere), Nowhere)),
                       List(AstReturnInstruction(quux, AstUnitValue(Nowhere), Nowhere)),
                       loc)
    val expected = Block(foo + bar, List(foo + baz), List(foo + quux), loc)
    testDefinition(expected, ast)
  }

  @Test
  def function = {
    val List(a, b, c) = List("a", "b", "c").map(new Symbol(_))
    val ast = AstFunction(foo,
                          AstUnitType(Nowhere),
                          List(AstTypeParameter(a, None, None, Nowhere)),
                          List(AstParameter(b, AstUnitType(Nowhere), Nowhere)),
                          List(AstBlock(bar,
                                        List(AstParameter(c, AstUnitType(Nowhere), Nowhere)),
                                        List(AstReturnInstruction(baz, 
                                                                  AstUnitValue(Nowhere),
                                                                  Nowhere)),
                                        Nowhere)),
                          loc)
    val expected = Function(foo, List(foo + a), List(foo + b), UnitType(), List(foo + bar), loc)
    testDefinition(expected, ast)
  }   
}
