package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class ValidationTest {
  def programFromCode(code: String): String = {
    "is64bit: true\n" +
    "function unit @main {\n" +
    "  block %entry {\n" +
    "    " + code + "\n" +
    "    return ()\n" +
    "  }\n" +
    "}\n"
  }

  def programContainsError[T <: CompileException](program: String)(implicit m: Manifest[T]) = {
    val errors = compileString(program).validate
    containsError[T](errors)
  }

  def codeContainsError[T <: CompileException](code: String)(implicit m: Manifest[T]) = {
    val program = programFromCode(code)
    programContainsError[T](program)
  }

  def containsError[T <: CompileException](errors: List[CompileException])
                                          (implicit m: Manifest[T]) =
  {
    assertTrue(errors.exists(m.erasure.isInstance(_)))
  }

  def programIsCorrect(program: String) = {
    val errors = compileString(program).validate
    assertTrue(errors.isEmpty)
  }

  def codeIsCorrect(code: String) = {
    val program = programFromCode(code)
    programIsCorrect(program)
  }

  @Test
  def stagedValidation {
    def f(x: Int) = if (x > 0) List(1, 2, 3) else throw new RuntimeException("stage failed")
    stage(f(1), f(-1))
    ()
  }

  @Test
  def emptyBlockTermination {
    val program = "function unit @main( ) { block %empty }"
    programContainsError[EmptyComponentsException](program)
  }

  @Test
  def blockTermination {
    val program = "function unit @main { block %entry { scall %foo = @main() } }"
    programContainsError[BlockTerminationException](program)
  }

  @Test
  def earlyTermination {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    return %a = ()\n" +
                  "    return %b = ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[EarlyTerminationException](program)
  }

  @Test
  def exitTermination {
    val program = "function unit @main { block %entry { intrinsic %foo = exit(int32 12) } }"
    val errors = compileString(program).validate
    assertTrue(errors.isEmpty)
  }

  @Test
  def returnTypeMismatch {
    val program = "function unit @main( ) { block %entry { return int32 12 } }"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def duplicateComponent {
    val (instName, blockName) = (Symbol("ret"), Symbol("block"))
    val inst = ReturnInstruction(instName, UnitValue)
    val block = Block(blockName, Nil, List(instName, instName))
    val function = Function(Symbol("main"), Nil, UnitType, List(blockName))
    var module = (new Module).add(inst, block, function)
    containsError[DuplicateComponentException](module.validate)
  }

  @Test
  def nonLocalAssign {
    val program = "global unit @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    assign %a = unit* @foo\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programIsCorrect(program)
  }

  @Test
  def nonLiteralGlobal {
    val (foo, bar) = (Symbol("foo"), Symbol("bar"))
    val gfoo = Global(foo, UnitType, Some(UnitValue))
    val gbar = Global(bar, UnitType, Some(DefinedValue(foo, UnitType)))
    val module = (new Module).add(gfoo, gbar)
    containsError[GlobalValueNonLiteralException](gbar.validate(module))
  }

  @Test
  def globalTypeMismatch {
    val program = "global unit @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    store unit* @foo, int32 12\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def binopNonNumeric {
    val code = "binop %a = () + ()"
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def binopMismatch {
    val code = "binop %a = int32 12 + int8 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def assignOutOfOrder {
    val code = "assign %a = unit %b\n" +
               "assign %b = ()"
    codeContainsError[InstructionOrderException](code)
  }

  @Test
  def relopMismatch {
    val code = "relop %a = int32 12 == ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def relopNonNumeric {
    val code = "relop %a = () < ()"
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def nonExistantBranchCondition {
    val code = "cond %a = true ? %foo() : %bar()"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def nonBooleanCondition {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond %a = int32 12 ? @main.foo( ) : @main.bar( )\n" +
                  "  }\n" +
                  "  block %foo { return () }\n" +
                  "  block %bar { return () }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def conditionArgumentCount {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond %a = true ? @main.foo(int32 12) : @main.bar(int32 12)\n" +
                  "  }\n" +
                  "  block %foo(int32 %x) { return () }\n" +
                  "  block %bar { return () }\n" +
                  "}\n"
    programContainsError[FunctionArgumentCountException](program)
  }

  @Test
  def conditionTypeMismatch {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond %a = true ? @main.foo(int32 12) : @main.bar(int32 12)\n" +
                  "  }\n" +
                  "  block %foo(int32 %x) { return () }\n" +
                  "  block %bar(boolean %x) { return () }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def conditionDuplicateBlock {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond %a = true ? %entry( ) : %entry( )\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[DuplicateComponentException](program)
  }

  @Test
  def staticCallMissingFunction {
    val code = "scall %c = @foo( )"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def entryBlockWithParameters {
    val program = "function unit @main { block %entry(unit %u) { return () } }"
    programContainsError[EntryParametersException](program)
  }

  @Test
  def floatBitOp {
    val code = "binop %a = float64 1. & float64 2."
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def upcastToNonPointer {
    val code = "upcast %a = null to int32"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastFromNonPointer {
    val code = "upcast %a = int32 12 to int32*"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastDown {
    val code = "upcast %a = null to int32*" + 
               "upcast %b = int32* %a to nulltype"
    codeContainsError[UpcastException](code)
  }

  @Test
  def stackAllocateInt {
    val code = "stack %a = int32"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackAllocateNull {
    val code = "stack %a = nulltype"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateInt {
    val code = "heap %a = int32"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateNull {
    val code = "heap %a = nulltype"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadInt {
    val code = "load %a = int32 12"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadNull {
    val code = "load %a = null"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeInt {
    val code = "store %a = int32 12, int32 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeNull {
    val code = "store %a = null, int32 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def paramsInNonEntryBlockCorrect {
    val program = "function unit @main { block %entry { return () } }\n" +
                  "function unit @f(unit %x) {\n" +
                  "  block %b1 {\n" +
                  "    branch @f.b2( )\n" +
                  "  }\n" +
                  "  block %b2( ) {\n" +
                  "    return unit @f.x\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def mainWithParameters {
    val program = "function unit @main(unit %x) { block %entry { return () } }"
    programContainsError[MainNonEmptyParametersException](program)
  }

  @Test
  def mainReturnType {
    val program = "function int32 @main { block %entry { return int32 12 } }"
    programContainsError[MainReturnTypeException](program)
  }

  @Test
  def upcastSizelessArray {
    val code = "upcast %a = [2 x int32] {int32 12, int32 34} to [? x int32]"
    codeIsCorrect(code)
  }

  @Test
  def loadElementArrayOutOfBounds {
    val code = "loadelement %a = [2 x int32] {int32 12, int32 34}, int64 5"
    codeIsCorrect(code)
  }

  @Test
  def loadElementBadIndexType {
    val code = "loadelement %a = [2 x int32] {int32 12, int32 34}, int32 5"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadElementTooManyIndices {
    val code = "loadelement %a = [2 x int32] {int32 12, int32 34}, int64 5, int64 6"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def storeType {
    val code = "storeelement [2 x int32] {int32 12, int32 64}, int64 0, ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackArrayAllocCountType {
    val code = "stackarray %a = () x unit"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressNonPointer {
    val code = "address %a = (), int64 0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressBadIndexType {
    val code = "stackarray %a = int64 5 x unit\n" +
               "address %b = [? x unit]* %a, int32 1"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressTooManyIndices {
    val code = "stackarray %a = int64 5 x unit\n" +
               "address %b = [? x unit]* %a, int64 1, int64 1"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def addressTwice {
    val code = "stackarray %a = int64 2 x [2 x int32]\n" +
               "address %b = [? x [? x int32]]* %a, int64 1, int64 1\n" +
               "store int32* %b, int32 12"
    codeIsCorrect(code)
  }

  @Test
  def globalAddress {
    val program = "is64bit: true\n" +
                  "global [2 x int32] @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    address %a = [2 x int32]* @foo, int64 1\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programIsCorrect(program)
  }

  @Test
  def duplicateStructField {
    val field = Field("foo", UnitType)
    val struct = Struct("bar", List(field.name, field.name))
    val module = (new Module).add(field, struct)
    val errors = struct.validateComponents(module)
    containsError[DuplicateComponentException](errors)
  }

  @Test
  def invalidStructValueCount {
    val program = "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    assign %x = struct @A {(), ()}\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[FieldCountException](program)
  }

  @Test
  def structValueFieldType {
    val program = "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    assign %x = struct @A {int32 12}\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def loadElementFromStruct {
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    loadelement %c = struct @A {()}, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def loadElementNonLiteralIndex {
    val program = "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    assign %i = int64 0\n" +
                  "    loadelement %e = struct @A {()}, int64 %i\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidIndexException](program)
  }

  @Test
  def storeElementDoubleIndex {
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field int32 %x\n" +
                  "}\n" +
                  "struct @B {\n" +
                  "  field struct @A %y\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    assign %a = struct @B { struct @A {int32 12} }\n" +
                  "    storeelement int32 34, struct @B %a, int64 0, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def nonExistantStructValue {
    val i1 = AssignInstruction("i1", StructValue("A", Nil))
    val i2 = ReturnInstruction("i2", UnitValue)
    val block = Block("main.entry", Nil, List(i1, i2).map(_.name))
    val function = Function("main", Nil, UnitType, List(block.name))
    val module = (new Module).add(i1, i2, block, function)
    val errors = module.validate
    containsError[UndefinedSymbolException](errors)
  }

  @Test
  def singleCyclicStruct {
    val program = "struct @A { field struct @A %x }\n" +
                  "function unit @main { block %entry { return () } }\n"
    programContainsError[CyclicStructException](program)
  }

  @Test
  def doubleCyclicStruct {
    val program = "struct @A { field struct @B %x }\n" +
                  "struct @B { field struct @A %y }\n" +
                  "function unit @main { block %entry { return () } }\n"
    programContainsError[CyclicStructException](program)
  }

  @Test
  def programMissingMain {
    val module = new Module(ty = ModuleType.PROGRAM)
    val errors = module.validateProgram
    containsError[MissingMainException](errors)
  }

  @Test
  def externalDefinition {
    val program = "function int32 @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    scall %x = @foo( )\n" +
                  "    intrinsic exit(int32 %x)\n" +
                  "  }\n" +
                  "}\n"
    val module = compileString(program)
    val errors = module.validateProgram
    containsError[ExternalDefinitionException](errors)
  }

  @Test
  def duplicateDependency {
    val module = new Module(dependencies = List(ModuleDependency("a", Version.MIN, Version.MAX),
                                                ModuleDependency("a", Version.MIN, Version.MAX)))
    val errors = module.validate
    containsError[DuplicateDependencyException](errors)
  }

  @Test
  def integerTruncate {
    codeContainsError[TypeMismatchException]("itruncate %a = () to int8")
    codeContainsError[TypeMismatchException]("itruncate %a = int64 12 to unit")
    codeContainsError[NumericTruncationException]("itruncate %a = int8 12 to int64")
  }

  @Test
  def integerExtend {
    codeContainsError[NumericExtensionException]("isextend %a = int64 12 to int8")
    codeContainsError[NumericExtensionException]("izextend %a = int64 12 to int8")
  }

  @Test
  def floatTruncate {
    codeContainsError[TypeMismatchException]("ftruncate %a = () to float32")
    codeContainsError[TypeMismatchException]("ftruncate %a = float64 3.2 to unit")
    codeContainsError[NumericTruncationException]("ftruncate %a = float32 3.2 to float64")
  }

  @Test
  def floatExtend {
    codeContainsError[NumericExtensionException]("fextend float64 3.2 to float32")
  }

  @Test
  def floatIntCast {
    codeContainsError[TypeMismatchException]("itof () to float32")
    codeContainsError[TypeMismatchException]("itof int32 12 to unit")
    codeContainsError[TypeMismatchException]("ftoi () to int32")
    codeContainsError[TypeMismatchException]("ftoi float64 3.4 to unit")
  }

  @Test
  def indexTypeIn32Bit {
    val program = "is64bit: false\n" +
                  "function unit @main {\n" +
                  "  block %entry( ) {\n" +
                  "    stack %a = [3 x unit]*\n" +
                  "    address %b = [3 x unit]* %a, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }
}
