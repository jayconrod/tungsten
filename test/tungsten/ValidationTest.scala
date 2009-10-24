package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class ValidationTest {
  def programContainsError[T <: CompileException](program: String)(implicit m: Manifest[T]) = {
    val errors = compileString(program).validate
    containsError[T](errors)
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
}
