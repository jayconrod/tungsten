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
  def booleanType = {
    assertEquals(BooleanType(loc), AstBooleanType(loc).compile(ctx))
  }

  @Test
  def intType = {
    assertEquals(IntType(32, loc), AstIntType(32, loc).compile(ctx))
  }

  @Test
  def floatType = {
    assertEquals(FloatType(32, loc), AstFloatType(32, loc).compile(ctx))
    assertEquals(FloatType(64, loc), AstFloatType(64, loc).compile(ctx))
  }

  @Test
  def pointerType = {
    assertEquals(PointerType(UnitType(), loc), 
                 AstPointerType(AstUnitType(Nowhere), loc).compile(ctx))
  }

  @Test
  def nullType = {
    assertEquals(NullType(loc), AstNullType(loc).compile(ctx))
  }

  @Test
  def arrayType = {
    assertEquals(ArrayType(Some(12), UnitType(), loc),
                 AstArrayType(Some(12), AstUnitType(Nowhere), loc).compile(ctx))
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
  def booleanValue = {
    assertEquals(BooleanValue(true, loc), AstBooleanValue(true, loc).compile(ctx))
  }

  @Test
  def intValues = {
    assertEquals(Int8Value(12, loc), AstInt8Value(12, loc).compile(ctx))
    assertEquals(Int16Value(12, loc), AstInt16Value(12, loc).compile(ctx))
    assertEquals(Int32Value(12, loc), AstInt32Value(12, loc).compile(ctx))
    assertEquals(Int64Value(12, loc), AstInt64Value(12, loc).compile(ctx))
  }

  @Test
  def floatValues = {
    assertEquals(Float32Value(12.3f, loc), AstFloat32Value(12.3f, loc).compile(ctx))
    assertEquals(Float64Value(12.3, loc), AstFloat64Value(12.3, loc).compile(ctx))
  }

  @Test
  def arrayValues = {
    assertEquals(ArrayValue(UnitType(), Nil, loc),
                 AstArrayValue(AstUnitType(Nowhere), Nil, loc).compile(ctx))
    assertEquals(ArrayValue(UnitType(), List(UnitValue()), loc),
                 AstArrayValue(AstUnitType(Nowhere), List(AstUnitValue(Nowhere)), loc).compile(ctx))
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
  def addressInst = {
    ctx.names.push(foo)
    val ast = AstAddressInstruction(bar,
                                    AstUnitValue(Nowhere),
                                    List(AstUnitValue(Nowhere), AstUnitValue(Nowhere)),
                                    loc)
    val expected = AddressInstruction(foo + bar,
                                      UnitValue(),
                                      List(UnitValue(), UnitValue()),
                                      loc)
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
  def binopInst = {
    ctx.names.push(foo)
    val ast = AstBinaryOperatorInstruction(bar,
                                           BinaryOperator.ADD,
                                           AstInt32Value(12, Nowhere),
                                           AstInt32Value(34, Nowhere),
                                           loc)
    val expected = BinaryOperatorInstruction(foo + bar,
                                             BinaryOperator.ADD,
                                             Int32Value(12),
                                             Int32Value(34),
                                             loc)
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
  def condBranchInst = {
    ctx.names.push(foo)
    val bazBlock = Block(foo + baz, Nil, Nil, Nowhere)
    ctx.module.add(bazBlock)
    val quuxBlock = Block(foo + quux, Nil, Nil, Nowhere)
    ctx.module.add(quuxBlock)
    val ast = AstConditionalBranchInstruction(bar,
                                              AstUnitValue(Nowhere),
                                              baz,
                                              List(AstInt32Value(12, Nowhere)),
                                              quux,
                                              List(AstInt32Value(34, Nowhere)),
                                              loc)
    val expected = ConditionalBranchInstruction(foo + bar,
                                                UnitValue(),
                                                bazBlock.name,
                                                List(Int32Value(12)),
                                                quuxBlock.name,
                                                List(Int32Value(34)),
                                                loc)
    testDefinition(expected, ast)
  }

  @Test
  def globalLoadInst = {
    val global = Global(foo, UnitType(), None)
    ctx.module.add(global)
    ctx.names.push(bar)
    val ast = AstGlobalLoadInstruction(baz, foo, loc)
    val expected = GlobalLoadInstruction(bar + baz, foo, loc)
    testDefinition(expected, ast)
  }

  @Test
  def globalStoreInst = {
    val global = Global(foo, UnitType(), None)
    ctx.module.add(global)
    ctx.names.push(bar)
    val ast = AstGlobalStoreInstruction(baz, foo, AstInt32Value(12, Nowhere), loc)
    val expected = GlobalStoreInstruction(bar + baz, foo, Int32Value(12), loc)
    testDefinition(expected, ast)
  }

  @Test
  def indirectCallInst = {
    ctx.names.push(foo)
    val ast = AstIndirectCallInstruction(bar, 
                                         AstSymbolValue(baz, Nowhere), 
                                         List(AstInt32Value(12, Nowhere)),
                                         loc)
    val expected = IndirectCallInstruction(foo + bar, 
                                           DefinedValue(baz),
                                           List(Int32Value(12)),
                                           loc)
    testDefinition(expected, ast)
  }

  @Test
  def loadInst = {
    ctx.names.push(foo)
    val ast = AstLoadInstruction(bar, AstUnitValue(Nowhere), loc)
    val expected = LoadInstruction(foo + bar, UnitValue(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def loadElementInst = {
    ctx.names.push(foo)
    val ast = AstLoadElementInstruction(bar,
                                        AstUnitValue(Nowhere),
                                        List(AstUnitValue(Nowhere), AstUnitValue(Nowhere)),
                                        loc)
    val expected = LoadElementInstruction(foo + bar,
                                          UnitValue(), 
                                          List(UnitValue(), UnitValue()),
                                          loc)
    testDefinition(expected, ast)
  }

  @Test
  def relopInst = {
    ctx.names.push(foo)
    val ast = AstRelationalOperatorInstruction(bar,
                                               RelationalOperator.EQUAL,
                                               AstInt32Value(12, Nowhere),
                                               AstInt32Value(34, Nowhere),
                                               loc)
    val expected = RelationalOperatorInstruction(foo + bar,
                                                 RelationalOperator.EQUAL,
                                                 Int32Value(12),
                                                 Int32Value(34),
                                                 loc)
    testDefinition(expected, ast)
  }

  @Test
  def stackAllocateInst = {
    ctx.names.push(foo)
    val ast = AstStackAllocateInstruction(bar, 
                                          AstPointerType(AstUnitType(Nowhere), Nowhere), 
                                          loc)
    val expected = StackAllocateInstruction(foo + bar, PointerType(UnitType()), loc)
    testDefinition(expected, ast)
  }

  @Test
  def stackArrayAllocInst = {
    ctx.names.push(foo)
    val ast = AstStackAllocateArrayInstruction(bar,
                                               AstUnitValue(Nowhere),
                                               AstUnitType(Nowhere),
                                               loc)
    val expected = StackAllocateArrayInstruction(foo + bar,
                                                 UnitValue(),
                                                 UnitType(),
                                                 loc)
    testDefinition(expected, ast)
  }

  @Test
  def staticCallInst = {
    ctx.names.push(foo)
    val baz = new Symbol("baz")
    val ast = AstStaticCallInstruction(bar, 
                                       baz,
                                       List(AstInt32Value(12, Nowhere), 
                                            AstInt32Value(34, Nowhere)),
                                       loc)
    val expected = StaticCallInstruction(foo + bar,
                                         baz,
                                         List(Int32Value(12), Int32Value(34)),
                                         loc)
    testDefinition(expected, ast)
  }

  @Test
  def storeInst = {
    ctx.names.push(foo)
    val ast = AstStoreInstruction(bar, AstUnitValue(Nowhere), AstUnitValue(Nowhere), loc)
    val expected = StoreInstruction(foo + bar, UnitValue(), UnitValue(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def storeElementInst = {
    ctx.names.push(foo)
    val ast = AstStoreElementInstruction(bar,
                                         AstUnitValue(Nowhere),
                                         List(AstUnitValue(Nowhere), AstUnitValue(Nowhere)),
                                         AstUnitValue(Nowhere),
                                         loc)
    val expected = StoreElementInstruction(foo + bar,
                                           UnitValue(),
                                           List(UnitValue(), UnitValue()),
                                           UnitValue(),
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
  def upcastInst = {
    ctx.names.push(foo)
    val ast = AstUpcastInstruction(bar, AstNullValue(Nowhere), AstNullType(Nowhere), loc)
    val expected = UpcastInstruction(foo + bar, NullValue(), NullType(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def block = {
    ctx.names.push(foo)
    val ast = AstBlock(bar,
                       List(AstParameter(baz, AstUnitType(Nowhere), Nowhere)),
                       List(AstReturnInstruction(quux, AstUnitValue(Nowhere), Nowhere)),
                       loc)
    val expected = Block(foo + bar, List(foo + bar + baz), List(foo + bar + quux), loc)
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
