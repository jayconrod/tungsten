package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class ValidationTest {
  def programContainsError[T <: CompileException](program: String)(implicit m: Manifest[T]) = {
    val errors = compileString(program).validate
    assertTrue(errors.exists(m.erasure.isInstance(_)))
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
}
