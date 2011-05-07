package tungsten

import collection.immutable.TreeMap
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import Utilities._

trait ValidationTest {
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
}
