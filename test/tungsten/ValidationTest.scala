package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class ValidationTest {
  def programContainsError[T <: CompileException](program: String)(implicit m: Manifest[T]) = {
    val errors = compileString(program).validate
    containsError[T](errors)
  }

  def codeContainsError[T <: CompileException](code: String)(implicit m: Manifest[T]) = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    " + code + "\n" +
                  "    #return r = ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[T](program)
  }

  def containsError[T <: CompileException](errors: List[CompileException])
                                          (implicit m: Manifest[T]) =
  {
    assertTrue(errors.exists(m.erasure.isInstance(_)))
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
    programContainsError[EmptyBlockException](program)
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
    val inst = ReturnInstruction(new Symbol("ret"), UnitValue())
    val block = Block(new Symbol("block"), Nil, List(inst.name, inst.name))
    val module = new Module
    List(inst, block).foreach(module.add(_))
    containsError[DuplicateComponentException](block.validate(module))
  }

  @Test
  def nonLocalAssign = {
    val (foo, bar) = (new Symbol("foo"), new Symbol("bar"))
    val global = Global(foo, UnitType(), None)
    val inst = AssignInstruction(bar, DefinedValue(foo))
    val module = new Module
    module.add(global)
    module.add(inst)
    containsError[InappropriateSymbolException](inst.validate(module))
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
  def entryBlockWrongParameters = {
    val inst = ReturnInstruction(new Symbol("r"), UnitValue())
    val param = Parameter(new Symbol("p"), UnitType())
    val block = Block(new Symbol("b"), Nil, List(inst.name))
    val function = Function(new Symbol("f"), Nil, List(param.name), 
                            UnitType(), List(block.name))
    val module = new Module
    module.add(inst)
    module.add(param)
    module.add(block)
    module.add(function)
    containsError[EntryParametersException](module.validate)
  }

  @Test
  def floatBitOp = {
    val code = "#binop a = 1. & 2."
    codeContainsError[FloatBitOperationException](code)
  }
}
