package tungsten

import collection.immutable.TreeMap
import org.junit.Test
import org.junit.Ignore
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
    val program = "function unit @main { block %entry { unit %foo = scall @main() } }"
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
    val function = Function(Symbol("main"), UnitType, Nil, Nil, List(blockName))
    var module = (new Module).add(inst, block, function)
    containsError[DuplicateComponentException](module.validate)
  }

  @Test
  def globalUse {
    val program = "is64bit: true\n" +
                  "global unit @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    unit* %a = address unit* @foo, int64 0\n" +
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
    val code = "unit %a = binop () + ()"
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def binopMismatch {
    val code = "int32 %a = binop int32 12 + int8 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def useOutOfOrder {
    val code = "int64 %a = binop int64 1 + int64 %b\n" +
               "int64 %b = binop int64 2 + int64 3\n"
    codeContainsError[InstructionOrderException](code)
  }

  @Test
  def relopMismatch {
    val code = "boolean %a = relop int32 12 == ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def relopNonNumeric {
    val code = "boolean %a = relop () < ()"
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
    val code = "unit %c = scall @foo( )"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def entryBlockWithParameters {
    val program = "function unit @main { block %entry(unit %u) { return () } }"
    programContainsError[EntryParametersException](program)
  }

  @Test
  def floatBitOp {
    val code = "float64 %a = binop float64 1. & float64 2."
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def upcastToNonPointer {
    val code = "int32 %a = upcast null"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastFromNonPointer {
    val code = "int32* %a = upcast int32 12"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastDown {
    val code = "int32* %a = upcast null" + 
               "nulltype %b = upcast int32* %a"
    codeContainsError[UpcastException](code)
  }

  @Test
  def stackAllocateInt {
    val code = "int32 %a = stack"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackAllocateNull {
    val code = "nulltype %a = stack"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateInt {
    val code = "int32 %a = heap"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateNull {
    val code = "nulltype %a = heap"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadInt {
    val code = "int32 %a = load int32 12"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadNull {
    val code = "unit %a = load null"
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
  def extractArrayOutOfBounds {
    val code = "int32 %a = extract [2 x int32] {int32 12, int32 34}, int64 5"
    codeIsCorrect(code)
  }

  @Test
  def extractBadIndexType {
    val code = "int32 %a = extract [2 x int32] {int32 12, int32 34}, int32 5"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def extractTooManyIndices {
    val code = "int32 %a = extract [2 x int32] {int32 12, int32 34}, int64 5, int64 6"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def insertType {
    val code = "insert (), [2 x int32] {int32 12, int32 64}, int64 0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackArrayAllocCountType {
    val code = "unit* %a = stackarray ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressNonPointer {
    val code = "unit* %a = address (), int64 0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressBadIndexType {
    val code = "unit* %a = stackarray int64 5\n" +
               "unit* %b = address unit* %a, int32 1"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressTooManyIndices {
    val code = "unit* %a = stackarray int64 5\n" +
               "unit* %b = address unit* %a, int64 1, int64 1"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def addressTwice {
    val code = "[2 x int32]* %a = stackarray int64 2\n" +
               "int32* %b = address [2 x int32]* %a, int64 1, int64 1\n" +
               "store int32 12, int32* %b"
    codeIsCorrect(code)
  }

  @Test
  def globalAddress {
    val program = "is64bit: true\n" +
                  "global [2 x int32] @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    int32* %a = address [2 x int32]* @foo, int64 0, int64 1\n" +
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
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    unit %x = extract struct @A {(), ()}, int64 0\n" +
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
                  "    unit %x = extract struct @A {int32 12}, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def extractFromStruct {
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    unit %c = extract struct @A {()}, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def extractNonLiteralIndex {
    val program = "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    int64 %i = binop int64 0 + int64 0\n" +
                  "    unit %e = extract struct @A {()}, int64 %i\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidIndexException](program)
  }

  @Test
  def insertDoubleIndex {
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field int32 %x\n" +
                  "}\n" +
                  "struct @B {\n" +
                  "  field struct @A %y\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    struct @B %a = insert int32 34, struct @B { struct @A { int32 12 } }, int64 0, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def loadElementDoubleIndex {
    val code = "[1 x [1 x unit]]* %a = stack\n" +
               "unit %b = loadelement [1 x [1 x unit]]* %a, int64 0, int64 0, int64 0"
    codeIsCorrect(code)
  }

  @Test
  def storeElementDoubleIndex {
    val code = "[1 x [1 x unit]]* %a = stack\n" +
               "storeelement (), [1 x [1 x unit]]* %a, int64 0, int64 0, int64 0"
    codeIsCorrect(code)
  }

  @Test
  def nonExistantStructValue {
    val i1 = ExtractInstruction("i1", UnitType, StructValue("A", List(UnitValue)), List(IntValue(0, 64)))
    val i2 = ReturnInstruction("i2", UnitType, UnitValue)
    val block = Block("main.entry", Nil, List(i1, i2).map(_.name))
    val function = Function("main", UnitType, Nil, Nil, List(block.name))
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
  def cyclicClassInheritance {
    val program = "class @A <: class @B\n" +
                  "class @B <: class @A\n"
    programContainsError[CyclicInheritanceException](program)
  }

  @Test
  def cyclicInterfaceInheritance {
    val program = "class @A\n" +
                  "interface @B <: interface @C\n" +
                  "interface @C <: class @A {\n" +
                  "  interface @B\n" +
                  "}\n"
    programContainsError[CyclicInheritanceException](program)
  }

  @Test
  def cyclicClassInterfaceInheritance {
    val program = "class @A {\n" +
                  "  interface @I\n" +
                  "}\n" +
                  "class @B <: class @A\n" +
                  "interface @I <: class @B\n"
    programContainsError[CyclicInheritanceException](program)
  }

  @Test
  def cyclicTypeParameter {
    val program = "class @A[type @T <: type @S, type @S <: type @T]"
    programContainsError[CyclicTypeParameterException](program)
  }

  @Test
  def illegalInterfaceInheritance {
    val program = "class @A\n" +
                  "class @B <: class @A\n" +
                  "interface @I <: class @B\n" +
                  "interface @J <: class @A {\n" +
                  "  interface @I\n" +
                  "}\n"
    programContainsError[IllegalInheritanceException](program)
  }

  @Test
  def illegalClassInheritance {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B <: class @R {\n" +
                  "  interface @I\n" +
                  "}\n" +
                  "interface @I <: class @A\n"
    programContainsError[IllegalInheritanceException](program)
  }

  @Test
  def illegalTypeInheritance { 
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B <: class @R\n" +
                  "class @C[type @T] <: class @R\n" +
                  "interface @I <: class @C[class @A]\n" +
                  "interface @J <: class @C[class @B] {\n" +
                  "  interface @I\n" +
                  "}\n"
    programContainsError[InheritanceConflictException](program)
  }

  @Test
  def conflictingInheritance {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B <: class @R\n" +
                  "interface @I[type @T] <: class @R\n" +
                  "interface @J <: interface @I[class @A]\n" +
                  "interface @K <: interface @J {\n" +
                  "  interface @I[class @B]\n" +
                  "}\n"
    programContainsError[InheritanceConflictException](program)
  }

  @Test
  def inheritedFieldType {
    val program = "class @A {\n" +
                  "  field unit @x\n" +
                  "}\n" +
                  "class @B <: class @A {\n" +
                  "  field boolean @y\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def missingField {
    val program = "class @A { field unit %x }\n" +
                  "class @B <: class @A"
    programContainsError[MissingFieldException](program)
  }

  @Test
  def missingThisParameter {
    val program = "class @A { methods { @f } }\n" +
                  "function unit @f()\n"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  def invalidThisParameter {
    val program = "class @A { methods { @f } }\n" +
                  "function unit @f(int64 %this)"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  @Ignore
  def invalidThisParameterTypeArgs {
    val program = "class @R\n" +
                  "class @A[type %T] <: class @R\n" +
                  "class @B[type %T] <: class @R { methods { %f } }\n" +
                  "function unit @B.f[type %T](class @B[class @A[type %T]] %this)"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  def methodFromUnrelatedClass {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { @A.f } }\n" +
                  "class @B <: class @R { methods { @A.f } }\n" +
                  "function unit @A.f(class @A %this)"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  def invalidInheritedMethod {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { @A.f } }\n" +
                  "function unit @A.f(class @R %this)"
    programContainsError[MethodNotInheritedException](program)
  }

  @Test
  def missingMethod {
    val program = "class @A { methods { @A.f } }\n" +
                  "class @B <: class @A\n" +
                  "function unit @A.f(class @A %this)"
    programContainsError[MissingMethodException](program)
  }

  @Test
  def inheritedMethodTypeFromClass {
    val program = "class @A {\n" +
                  "  methods {\n" +
                  "    @A.f\n" +
                  "  }\n" +
                  "}\n" +
                  "class @B <: class @A {\n" +
                  "  methods {\n" +
                  "    @B.f\n" +
                  "  }\n" +
                  "}\n" +
                  "function unit @A.f(class @A %this, unit %x)\n" +
                  "function unit @B.f(class @A %this)\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def inheritedMethodTypeFromInterface {
    val program = "class @R\n" +
                  "interface @I <: class @R { methods { @I.f } }\n" +
                  "class @A <: class @R {\n" +
                  "  interface @I { @A.f }\n" +
                  "  methods { @A.f }\n" +
                  "}\n" +
                  "function unit @I.f(interface @I %this, unit %x)\n" +
                  "function unit @A.f(class @A %this)\n"
    programContainsError[TypeMismatchException](program)
  }
 
  @Test
  def methodVariance {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @C <: class @R { methods { @C.f } }\n" +
                  "class @D <: class @C { methods { @D.f } }\n" +
                  "function class @R @C.f(class @C %this, class @A %x)\n" +
                  "function class @A @D.f(class @D %this, class @R %x)\n"
    programIsCorrect(program)
  }

  @Test
  def foreignMethodInInterface {
    val program = "class @R\n" +
                  "interface @I <: class @R { methods { @I.f } }\n" +
                  "class @C <: class @R { interface @I { @I.f } }\n" +
                  "function unit @I.f(class @R %this)\n"
    programContainsError[ForeignInterfaceMethodException](program)
  }

  @Test
  @Ignore
  def interfaceTypeAndMethodMismatch {
    val r = Class("R", Nil, None, Nil, Nil, Nil, Nil, Nil)
    val i = Interface("I", Nil, ClassType(r.name), Nil, Nil, Nil)
    val a = Class("A", Nil, Some(ClassType("A")), List(InterfaceType(i.name)), Nil, Nil, Nil, Nil)
    val definitions = TreeMap(r.name -> r, i.name -> i, a.name -> a)
    val module = new Module(definitions = definitions)
    val errors = module.validate
    containsError[InterfaceTypeMethodMismatchException](errors)
  }
  
  @Test
  def invalidTypeInClass {
    val program = "class @A <: class @B"
    programContainsError[UndefinedSymbolException](program)
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
                  "    int32 %x = scall @foo( )\n" +
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
    codeContainsError[TypeMismatchException]("int8 %a = itruncate ()")
    codeContainsError[TypeMismatchException]("unit %a = itruncate int64 12")
    codeContainsError[NumericTruncationException]("int64 %a = itruncate int8 12")
  }

  @Test
  def integerExtend {
    codeContainsError[NumericExtensionException]("int8 %a = isextend int64 12")
    codeContainsError[NumericExtensionException]("int8 %a = izextend int64 12")
  }

  @Test
  def floatTruncate {
    codeContainsError[TypeMismatchException]("float32 %a = ftruncate ()")
    codeContainsError[TypeMismatchException]("unit %a = ftruncate float64 3.2")
    codeContainsError[NumericTruncationException]("float64 %a = ftruncate float32 3.2")
  }

  @Test
  def floatExtend {
    codeContainsError[NumericExtensionException]("float32 %a = fextend float64 3.2")
  }

  @Test
  def floatIntCast {
    codeContainsError[TypeMismatchException]("float32 %a = itof ()")
    codeContainsError[TypeMismatchException]("unit %a = itof int32 12")
    codeContainsError[TypeMismatchException]("int32 %a = itof ()")
    codeContainsError[TypeMismatchException]("unit %a = itof float64 3.4")
  }

  @Test
  def indexTypeIn32Bit {
    val program = "is64bit: false\n" +
                  "function unit @main {\n" +
                  "  block %entry( ) {\n" +
                  "    [3 x unit]* %a = stack\n" +
                  "    unit* %b = address [3 x unit]* %a, int64 0\n" +
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

  @Test
  def bitcastCorrect {
    val code = "int64 %a = bitcast int64 0"
    codeIsCorrect(code)
  }

  @Test
  def bitcastSizes {
    val code = "int64 %a = bitcast int32 0"
    codeContainsError[InvalidBitCastException](code)
  }
}
