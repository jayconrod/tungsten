package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class ValidationTest {
  def programFromCode(code: String): String = {
    "#function main( ): #unit {\n" +
    "  #block entry( ) {\n" +
    "    " + code + "\n" +
    "    #return ()\n" +
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
  def stagedValidation = {
    def f(x: Int) = if (x > 0) List(1, 2, 3) else throw new RuntimeException("stage failed")
    stage(f(1), f(-1))
    ()
  }

  @Test
  def emptyBlockTermination = {
    val program = "#function main( ): #unit { #block empty( ) { } }"
    programContainsError[EmptyComponentsException](program)
  }

  @Test
  def blockTermination = {
    val program = "#function main( ): #unit { #block entry( ) { #scall foo = main( ) } }"
    programContainsError[BlockTerminationException](program)
  }

  @Test
  def earlyTermination = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #return a = ()\n" +
                  "    #return b = ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[EarlyTerminationException](program)
  }

  @Test
  def exitTermination = {
    val program = "#function main( ): #unit { #block entry( ) { #intrinsic foo = exit(12) } }"
    val errors = compileString(program).validate
    assertTrue(errors.isEmpty)
  }

  @Test
  def missingMain = {
    programContainsError[MissingMainException]("")    
  }

  @Test
  def returnTypeMismatch = {
    val program = "#function main( ): #unit { #block entry( ) { #return r = 12 } }"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def duplicateComponent = {
    val (instName, blockName) = (new Symbol("ret"), new Symbol("block"))
    val inst = ReturnInstruction(instName, UnitValue())
    val block = Block(blockName, Nil, List(instName, instName))
    val function = Function(new Symbol("main"), Nil, Nil, UnitType(), List(blockName))
    val module = new Module
    for (defn <- List(inst, block, function))
      module.add(defn)
    containsError[DuplicateComponentException](module.validate)
  }

  @Test
  def nonLocalAssign = {
    val program = "#global foo: #unit\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #assign a = foo\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[InappropriateSymbolException](program)
  }

  @Test
  def nonLiteralGlobal = {
    val (foo, bar) = (new Symbol("foo"), new Symbol("bar"))
    val gfoo = Global(foo, UnitType(), Some(UnitValue()))
    val gbar = Global(bar, UnitType(), Some(DefinedValue(foo)))
    val module = new Module
    module.add(gfoo)
    module.add(gbar)
    containsError[GlobalValueNonLiteralException](gbar.validate(module))
  }

  @Test
  def globalTypeMismatch = {
    val (foo, bar) = (new Symbol("foo"), new Symbol("bar"))
    val gfoo = Global(foo, UnitType(), None)
    val ibar = GlobalStoreInstruction(bar, foo, Int32Value(12))
    val module = new Module
    module.add(gfoo)
    module.add(ibar)
    containsError[TypeMismatchException](ibar.validate(module))
  }

  @Test
  def binopNonNumeric = {
    val code = "#binop a = () + ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def binopMismatch = {
    val code = "#binop a = 12 + 34b"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def assignOutOfOrder = {
    val code = "#assign a = b\n" +
               "#assign b = ()"
    codeContainsError[InstructionOrderException](code)
  }

  @Test
  def relopMismatch = {
    val code = "#relop a = 12 == ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def relopNonNumeric = {
    val code = "#relop a = () < ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def nonExistantCondition = {
    val code = "#cond a = #true ? foo : bar ( )"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def nonBooleanCondition = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #cond a = 12 ? foo : bar ( )\n" +
                  "  }\n" +
                  "  #block foo( ) { #return a = () }\n" +
                  "  #block bar( ) { #return a = () }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def conditionArgumentCount = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #cond a = #true ? foo : bar (12)\n" +
                  "  }\n" +
                  "  #block foo(x: #int32) { #return r = () }\n" +
                  "  #block bar( ) { #return r = () }\n" +
                  "}\n"
    programContainsError[FunctionArgumentCountException](program)
  }

  @Test
  def conditionTypeMismatch = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #cond a = #true ? foo : bar (12)\n" +
                  "  }\n" +
                  "  #block foo(x: #int32) { #return r = () }\n" +
                  "  #block bar(x: #boolean) { #return r = () }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def conditionDuplicateBlock = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #cond a = #true ? entry : entry ( )\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[DuplicateComponentException](program)
  }

  @Test
  def staticCallMissingFunction = {
    val code = "#scall c = foo( )"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def entryBlockWithParameters = {
    val program = "#function main( ): #unit { #block entry(u: #unit) { #return r = () } }"
    programContainsError[EntryParametersException](program)
  }

  @Test
  def floatBitOp = {
    val code = "#binop a = 1. & 2."
    codeContainsError[FloatBitOperationException](code)
  }

  @Test
  def upcastToNonPointer = {
    val code = "#upcast a = #null : #int32"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastFromNonPointer = {
    val code = "#upcast a = 12 : #int32*"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastDown = {
    val code = "#upcast a = #null : #int32*" + 
               "#upcast b = a : #null"
    codeContainsError[UpcastException](code)
  }

  @Test
  def stackAllocateInt = {
    val code = "#stack a : #int32"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackAllocateNull = {
    val code = "#stack a : #null"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadInt = {
    val code = "#load a = *12"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadNull = {
    val code = "#load a = *#null"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeInt = {
    val code = "#store a = *12 <- 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeNull = {
    val code = "#store a = *#null <- 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def paramsInNonEntryBlockCorrect = {
    val program = "#function main( ): #unit { #block entry( ) { #return r = () } }\n" +
                  "#function f(x: #unit): #unit {\n" +
                  "  #block b1( ) {\n" +
                  "    #branch t1 = b2( )\n" +
                  "  }\n" +
                  "  #block b2( ) {\n" +
                  "    #return r = x\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def mainWithParameters = {
    val program = "#function main(x: #unit): #unit { #block entry( ) { #return r = () } }"
    programContainsError[MainNonEmptyParametersException](program)
  }

  @Test
  def mainReturnType = {
    val program = "#function main( ): #int32 { #block entry( ) { #return r = 12 } }"
    programContainsError[MainReturnTypeException](program)
  }

  @Test
  def upcastSizelessArray = {
    val code = "#upcast a = [#int32: 12, 34] : [? * #int32]"
    codeIsCorrect(code)
  }

  @Test
  def loadArrayOutOfBounds = {
    val code = "#loadelement a = [#int32: 12, 34], 5L"
    codeIsCorrect(code)
  }

  @Test
  def loadBadIndexType = {
    val code = "#loadelement a = [#int32: 12, 34], 5"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def loadTooManyIndices = {
    val code = "#loadelement a = [#int32: 12, 34], 5, 6"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def storeType = {
    val code = "#storeelement [#int32: 12, 34], 0L <- ()"
    codeContainsError[TypeMismatchException](code)
  }
}
