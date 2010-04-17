package tungsten

import java.io.File
import org.junit.Test
import org.junit.Assert._
import Utilities._

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
  def headers {
    val ast = new AstModule("a.b#12",
                            ModuleType.LIBRARY,
                            new Version(List(1, 2, 3)),
                            None,
                            List(new ModuleDependency("c.d#34", 
                                                      new Version(List(0, 1)),
                                                      new Version(List(3, 4)))),
                            List(new File("/foo/bar")),
                            true,
                            Nil)
    val Left(module) = ast.compile
    val expected = new Module(module.name,
                              module.ty,
                              module.version,
                              None,
                              module.dependencies,
                              module.searchPaths,
                              true,
                              Map[Symbol, Definition]())                             
    assertEquals(expected, module)
  }

  @Test
  def unitType {
    assertEquals(UnitType(loc), AstUnitType(loc).compile(ctx))
  }

  @Test
  def booleanType {
    assertEquals(BooleanType(loc), AstBooleanType(loc).compile(ctx))
  }

  @Test
  def intType {
    assertEquals(IntType(32, loc), AstIntType(32, loc).compile(ctx))
  }

  @Test
  def floatType {
    assertEquals(FloatType(32, loc), AstFloatType(32, loc).compile(ctx))
    assertEquals(FloatType(64, loc), AstFloatType(64, loc).compile(ctx))
  }

  @Test
  def pointerType {
    assertEquals(PointerType(UnitType(), loc), 
                 AstPointerType(AstUnitType(Nowhere), loc).compile(ctx))
  }

  @Test
  def nullType {
    assertEquals(NullType(loc), AstNullType(loc).compile(ctx))
  }

  @Test
  def arrayType {
    assertEquals(ArrayType(Some(12), UnitType(), loc),
                 AstArrayType(Some(12), AstUnitType(Nowhere), loc).compile(ctx))
  }

  @Test
  def structType {
    val struct = Struct(foo, Nil)
    ctx.addDefn(struct)
    val ast = AstClassType(foo, Nil, loc)
    val expected = StructType(foo, loc)
    assertEquals(expected, ast.compile(ctx))
  }

  @Test
  def defaultType {
    val ast = AstClassType(foo, Nil, loc)
    val ty = ast.compileOrElse(ctx)
    assertFalse(ctx.errors.isEmpty)
    assertEquals(loc, ty.location)
  }

  @Test
  def unitValue {
    assertEquals(UnitValue(loc), AstUnitValue(loc).compile(ctx))
  }

  @Test
  def booleanValue {
    assertEquals(BooleanValue(true, loc), AstBooleanValue(true, loc).compile(ctx))
  }

  @Test
  def intValues {
    assertEquals(Int8Value(12, loc), AstInt8Value(12, loc).compile(ctx))
    assertEquals(Int16Value(12, loc), AstInt16Value(12, loc).compile(ctx))
    assertEquals(Int32Value(12, loc), AstInt32Value(12, loc).compile(ctx))
    assertEquals(Int64Value(12, loc), AstInt64Value(12, loc).compile(ctx))
  }

  @Test
  def floatValues {
    assertEquals(Float32Value(12.3f, loc), AstFloat32Value(12.3f, loc).compile(ctx))
    assertEquals(Float64Value(12.3, loc), AstFloat64Value(12.3, loc).compile(ctx))
  }

  @Test
  def arrayValues {
    assertEquals(ArrayValue(UnitType(), Nil, loc),
                 AstArrayValue(AstUnitType(Nowhere), Nil, loc).compile(ctx))
    assertEquals(ArrayValue(UnitType(), List(UnitValue()), loc),
                 AstArrayValue(AstUnitType(Nowhere), List(AstUnitValue(Nowhere)), loc).compile(ctx))
  }

  @Test
  def structValue {
    val field = Field("A.b", UnitType())
    ctx.addDefn(field)
    val struct = Struct("A", List(field.name))
    ctx.addDefn(struct)
    val ast = AstAggregateValue("A", List(AstUnitValue(Nowhere)), loc)
    val expected = StructValue("A", List(UnitValue()), loc)
    assertEquals(expected, ast.compile(ctx))
  }    

  @Test
  def symbolValue {
    ctx.names.push(foo)
    val param = Parameter(foo + bar, UnitType(Nowhere))
    ctx.addDefn(param)
    val ast = AstSymbolValue(bar, loc)
    val expected = DefinedValue(param.name, loc)
    assertEquals(expected, ast.compile(ctx))
  }

  @Test
  def globalDefn {
    val global = new Global(foo, UnitType(Nowhere), Some(UnitValue(Nowhere)), loc)
    val ast = AstGlobal(foo, AstUnitType(Nowhere), Some(AstUnitValue(Nowhere)), loc)
    ast.compile(ctx)
    assertEquals(global, get(foo))
  }

  @Test
  def parameter {
    ctx.names.push(foo)
    val expected = Parameter(foo + bar, UnitType(Nowhere), loc)
    val ast = AstParameter(bar, AstUnitType(Nowhere), loc)
    testDefinition(expected, ast)
  }

  @Test
  def addressInst {
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
  def assignInst {
    ctx.names.push(foo)
    val ast = AstAssignInstruction(bar, AstUnitValue(Nowhere), loc)
    val expected = AssignInstruction(foo + bar, UnitValue(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def binopInst {
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
  def branchInst {
    ctx.names.push(foo)
    val block = Block(foo + baz, Nil, Nil, Nowhere)
    ctx.addDefn(block)
    val ast = AstBranchInstruction(bar, baz, List(AstInt32Value(12, Nowhere)), loc)
    val expected = BranchInstruction(foo + bar, 
                                     block.name,
                                     List(Int32Value(12, Nowhere)),
                                     loc)
    testDefinition(expected, ast)
  }

  @Test
  def condBranchInst {
    ctx.names.push(foo)
    val bazBlock = Block(foo + baz, Nil, Nil, Nowhere)
    ctx.addDefn(bazBlock)
    val quuxBlock = Block(foo + quux, Nil, Nil, Nowhere)
    ctx.addDefn(quuxBlock)
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
  def floatExtendInst {
    ctx.names.push(foo)
    val ast = AstFloatExtendInstruction(bar,
                                        AstUnitValue(Nowhere),
                                        AstUnitType(Nowhere),
                                        loc)
    val expected = FloatExtendInstruction(foo + bar,
                                          UnitValue(),
                                          UnitType(),
                                          loc)
    testDefinition(expected, ast)
  }

  @Test
  def floatToIntInst {
    ctx.names.push(foo)
    val ast = AstFloatToIntegerInstruction(bar,
                                           AstUnitValue(Nowhere),
                                           AstUnitType(Nowhere),
                                           loc)
    val expected = FloatToIntegerInstruction(foo + bar,
                                             UnitValue(),
                                             UnitType(),
                                             loc)
    testDefinition(expected, ast)
  }

  @Test
  def floatTruncateInst {
    ctx.names.push(foo)
    val ast = AstFloatTruncateInstruction(bar,
                                        AstUnitValue(Nowhere),
                                        AstUnitType(Nowhere),
                                        loc)
    val expected = FloatTruncateInstruction(foo + bar,
                                          UnitValue(),
                                          UnitType(),
                                          loc)
    testDefinition(expected, ast)
  }

  @Test
  def heapAllocateInst {
    ctx.names.push(foo)
    val ast = AstHeapAllocateInstruction(bar,
                                         AstPointerType(AstUnitType(Nowhere), Nowhere),
                                         loc)
    val expected = HeapAllocateInstruction(foo + bar, PointerType(UnitType()), loc)
    testDefinition(expected, ast)
  }

  @Test
  def heapAllocateArrayInst {
    ctx.names.push(foo)
    val ast = AstHeapAllocateArrayInstruction(bar,
                                              AstUnitValue(Nowhere),
                                              AstUnitType(Nowhere),
                                              loc)
    val expected = HeapAllocateArrayInstruction(foo + bar,
                                                UnitValue(),
                                                UnitType(),
                                                loc)
    testDefinition(expected, ast)
  }

  @Test
  def indirectCallInst {
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
  def integerSignExtendInst {
    ctx.names.push(foo)
    val ast = AstIntegerSignExtendInstruction(bar,
                                              AstUnitValue(Nowhere),
                                              AstUnitType(Nowhere),
                                              loc)
    val expected = IntegerSignExtendInstruction(foo + bar,
                                                UnitValue(),
                                                UnitType(),
                                                loc)
    testDefinition(expected, ast)
  }

  @Test
  def integerToFloatInst {
    ctx.names.push(foo)
    val ast = AstIntegerToFloatInstruction(bar,
                                           AstUnitValue(Nowhere),
                                           AstUnitType(Nowhere),
                                           loc)
    val expected = IntegerToFloatInstruction(foo + bar,
                                             UnitValue(),
                                             UnitType(),
                                             loc)
    testDefinition(expected, ast)
  }

  @Test
  def integerTruncateInst {
    ctx.names.push(foo)
    val ast = AstIntegerTruncateInstruction(bar,
                                            AstUnitValue(Nowhere),
                                            AstUnitType(Nowhere),
                                            loc)
    val expected = IntegerTruncateInstruction(foo + bar,
                                              UnitValue(),
                                              UnitType(),
                                              loc)
    testDefinition(expected, ast)
  }

  @Test
  def integerZeroExtendInst {
    ctx.names.push(foo)
    val ast = AstIntegerZeroExtendInstruction(bar,
                                              AstUnitValue(Nowhere),
                                              AstUnitType(Nowhere),
                                              loc)
    val expected = IntegerZeroExtendInstruction(foo + bar,
                                                UnitValue(),
                                                UnitType(),
                                                loc)
    testDefinition(expected, ast)
  }

  @Test
  def loadInst {
    ctx.names.push(foo)
    val ast = AstLoadInstruction(bar, AstUnitValue(Nowhere), loc)
    val expected = LoadInstruction(foo + bar, UnitValue(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def loadElementInst {
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
  def relopInst {
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
  def stackAllocateInst {
    ctx.names.push(foo)
    val ast = AstStackAllocateInstruction(bar, 
                                          AstPointerType(AstUnitType(Nowhere), Nowhere), 
                                          loc)
    val expected = StackAllocateInstruction(foo + bar, PointerType(UnitType()), loc)
    testDefinition(expected, ast)
  }

  @Test
  def stackArrayAllocInst {
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
  def staticCallInst {
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
  def storeInst {
    ctx.names.push(foo)
    val ast = AstStoreInstruction(bar, AstUnitValue(Nowhere), AstUnitValue(Nowhere), loc)
    val expected = StoreInstruction(foo + bar, UnitValue(), UnitValue(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def storeElementInst {
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
  def returnInst {
    ctx.names.push(foo)
    val ast = AstReturnInstruction(bar, AstUnitValue(Nowhere), loc)
    val expected = ReturnInstruction(foo + bar, UnitValue(Nowhere), loc)
    testDefinition(expected, ast)
  }

  @Test
  def upcastInst {
    ctx.names.push(foo)
    val ast = AstUpcastInstruction(bar, AstNullValue(Nowhere), AstNullType(Nowhere), loc)
    val expected = UpcastInstruction(foo + bar, NullValue(), NullType(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def block {
    ctx.names.push(foo)
    val ast = AstBlock(bar,
                       List(AstParameter(baz, AstUnitType(Nowhere), Nowhere)),
                       List(AstReturnInstruction(quux, AstUnitValue(Nowhere), Nowhere)),
                       loc)
    val expected = Block(foo + bar, List(foo + bar + baz), List(foo + bar + quux), loc)
    testDefinition(expected, ast)
  }

  @Test
  def function {
    val List(a, b, c) = List("a", "b", "c").map(new Symbol(_))
    val ast = AstFunction(foo,
                          AstUnitType(Nowhere),
                          List(AstParameter(b, AstUnitType(Nowhere), Nowhere)),
                          List(AstBlock(bar,
                                        List(AstParameter(c, AstUnitType(Nowhere), Nowhere)),
                                        List(AstReturnInstruction(baz, 
                                                                  AstUnitValue(Nowhere),
                                                                  Nowhere)),
                                        Nowhere)),
                          loc)
    val expected = Function(foo, List(foo + b), UnitType(), List(foo + bar), loc)
    testDefinition(expected, ast)
  }

  @Test
  def field {
    ctx.names.push(foo)
    val ast = AstField(bar, AstUnitType(Nowhere), loc)
    val expected = Field(foo + bar, UnitType(), loc)
    testDefinition(expected, ast)
  }

  @Test
  def struct {
    val ast = AstStruct(foo,
                        List(AstField(bar, AstUnitType(Nowhere), Nowhere)),
                        loc)
    ctx.addDefn(Field(foo + bar, UnitType()))
    val expected = Struct(foo, List(foo + bar), loc)
    testDefinition(expected, ast)
  }

  @Test
  def undefinedClassType {
    val ast = AstClassType("A", Nil, Nowhere)
    ast.compile(ctx)
    assertTrue(ctx.errors.exists(_.isInstanceOf[UndefinedSymbolException]))
  }

  @Test
  def undefinedAggregateValue {
    val ast = AstAggregateValue("A", Nil, Nowhere)
    ast.compile(ctx)
    assertTrue(ctx.errors.exists(_.isInstanceOf[UndefinedSymbolException]))
  }
}