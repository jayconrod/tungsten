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
    val program = "function unit @main { block %entry { scall unit %foo = @main() } }"
    programContainsError[BlockTerminationException](program)
  }

  @Test
  def earlyTermination {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[EarlyTerminationException](program)
  }

  @Test
  def exitTermination {
    val program = "function unit @main { block %entry { intrinsic exit(int32 12) } }"
    programIsCorrect(program)
  }

  @Test
  def returnTypeMismatch {
    val program = "function unit @main( ) { block %entry { return int32 12 } }"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def duplicateComponent {
    val (instName, blockName) = (Symbol("ret"), Symbol("block"))
    val inst = ReturnInstruction(instName, UnitType, UnitValue)
    val block = Block(blockName, Nil, List(instName, instName))
    val function = Function(Symbol("main"), UnitType, Nil, List(blockName))
    var module = (new Module).add(inst, block, function)
    containsError[DuplicateComponentException](module.validate)
  }

  @Test
  def nonLocalAssign {
    val program = "global unit @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    assign unit* %a = unit* @foo\n" +
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
    val code = "binop unit %a = () + ()"
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def binopMismatch {
    val code = "binop int32 %a = int32 12 + int8 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def assignOutOfOrder {
    val code = "assign unit %a = unit %b\n" +
               "assign unit %b = ()"
    codeContainsError[InstructionOrderException](code)
  }

  @Test
  def relopMismatch {
    val code = "relop boolean %a = int32 12 == ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def relopNonNumeric {
    val code = "relop boolean %a = () < ()"
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def nonExistantBranchCondition {
    val code = "cond true ? %foo() : %bar()"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def nonBooleanCondition {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond int32 12 ? @main.foo( ) : @main.bar( )\n" +
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
                  "    cond true ? @main.foo(int32 12) : @main.bar(int32 12)\n" +
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
                  "    cond true ? @main.foo(int32 12) : @main.bar(int32 12)\n" +
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
                  "    cond true ? %entry( ) : %entry( )\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[DuplicateComponentException](program)
  }

  @Test
  def staticCallMissingFunction {
    val code = "scall unit %c = @foo( )"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def entryBlockWithParameters {
    val program = "function unit @main { block %entry(unit %u) { return () } }"
    programContainsError[EntryParametersException](program)
  }

  @Test
  def floatBitOp {
    val code = "binop float64 %a = float64 1. & float64 2."
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def upcastToNonPointer {
    val code = "upcast int32 %a = null"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastFromNonPointer {
    val code = "upcast int32* %a = int32 12"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastDown {
    val code = "upcast int32* %a = null" + 
               "upcast nulltype %b = int32* %a"
    codeContainsError[UpcastException](code)
  }

  @Test
  def stackAllocateInt {
    val code = "stack int32 %a"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackAllocateNull {
    val code = "stack nulltype %a"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateInt {
    val code = "heap int32 %a"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateNull {
    val code = "heap nulltype %a"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadInt {
    val code = "load int32 %a = int32 12"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadNull {
    val code = "load unit %a = null"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeInt {
    val code = "store int32 12, int32 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeNull {
    val code = "store int32 34, null"
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
    val code = "upcast [? x int32] %a = [2 x int32] {int32 12, int32 34}"
    codeIsCorrect(code)
  }

  @Test
  def loadElementArrayOutOfBounds {
    val code = "loadelement int32 %a = [2 x int32] {int32 12, int32 34}, int64 5"
    codeIsCorrect(code)
  }

  @Test
  def loadElementBadIndexType {
    val code = "loadelement int32 %a = [2 x int32] {int32 12, int32 34}, int32 5"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadElementTooManyIndices {
    val code = "loadelement int32 %a = [2 x int32] {int32 12, int32 34}, int64 5, int64 6"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def storeType {
    val code = "storeelement (), [2 x int32] {int32 12, int32 64}, int64 0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackArrayAllocCountType {
    val code = "stackarray [? x unit]* %a = () x unit"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressNonPointer {
    val code = "address unit* %a = (), int64 0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressBadIndexType {
    val code = "stackarray [? x unit]* %a = int64 5 x unit\n" +
               "address unit* %b = [? x unit]* %a, int32 1"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressTooManyIndices {
    val code = "stackarray [? x unit]* %a = int64 5 x unit\n" +
               "address unit* %b = [? x unit]* %a, int64 1, int64 1"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def addressTwice {
    val code = "stackarray [? x [2 x int32]]* %a = int64 2 x [2 x int32]\n" +
               "address int32* %b = [? x [2 x int32]]* %a, int64 1, int64 1\n" +
               "store int32 12, int32* %b"
    codeIsCorrect(code)
  }

  @Test
  def globalAddress {
    val program = "is64bit: true\n" +
                  "global [2 x int32] @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    address int32* %a = [2 x int32]* @foo, int64 1\n" +
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
                  "    assign struct @A %x = struct @A {(), ()}\n" +
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
                  "    assign struct @A %x = struct @A {int32 12}\n" +
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
                  "    loadelement unit %c = struct @A {()}, int64 0\n" +
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
                  "    assign int64 %i = int64 0\n" +
                  "    loadelement unit %e = struct @A {()}, int64 %i\n" +
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
                  "    assign struct @B %a = struct @B { struct @A {int32 12} }\n" +
                  "    storeelement int32 34, struct @B %a, int64 0, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def nonExistantStructValue {
    val i1 = AssignInstruction("i1", StructType("A"), StructValue("A", Nil))
    val i2 = ReturnInstruction("i2", UnitType, UnitValue)
    val block = Block("main.entry", Nil, List(i1, i2).map(_.name))
    val function = Function("main", UnitType, Nil, List(block.name))
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
                  "    scall int32 %x = @foo( )\n" +
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
    codeContainsError[TypeMismatchException]("itruncate int8 %a = ()")
    codeContainsError[TypeMismatchException]("itruncate unit %a = int64 12")
    codeContainsError[NumericTruncationException]("itruncate int64 %a = int8 12")
  }

  @Test
  def integerExtend {
    codeContainsError[NumericExtensionException]("isextend int8 %a = int64 12")
    codeContainsError[NumericExtensionException]("izextend int8 %a = int64 12")
  }

  @Test
  def floatTruncate {
    codeContainsError[TypeMismatchException]("ftruncate float32 %a = ()")
    codeContainsError[TypeMismatchException]("ftruncate unit %a = float64 3.2")
    codeContainsError[NumericTruncationException]("ftruncate float64 %a = float32 3.2")
  }

  @Test
  def floatExtend {
    codeContainsError[NumericExtensionException]("fextend float32 %a = float64 3.2")
  }

  @Test
  def floatIntCast {
    codeContainsError[TypeMismatchException]("itof float32 %a = ()")
    codeContainsError[TypeMismatchException]("itof unit %a = int32 12")
    codeContainsError[TypeMismatchException]("ftoi int32 %a = ()")
    codeContainsError[TypeMismatchException]("ftoi unit %a = float64 3.4")
  }

  @Test
  def indexTypeIn32Bit {
    val program = "is64bit: false\n" +
                  "function unit @main {\n" +
                  "  block %entry( ) {\n" +
                  "    stack [3 x unit]* %a\n" +
                  "    address unit* %b = [3 x unit]* %a, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def nonExistantAnnotation {
    val program = "@foo global unit @bar"
    programContainsError[UndefinedSymbolException](program)
  }

  @Test
  def annotationFieldCount {
    val program = "annotation @foo(unit %a)\n" +
                  "@foo global unit @bar"
    programContainsError[AnnotationArgumentCountException](program)
  }

  @Test
  def annotationFieldType {
    val program = "annotation @foo(unit %a)\n" +
                  "@foo(true) global unit @bar"
    programContainsError[TypeMismatchException](program)
  }
}
