package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import tungsten.ModuleIO
import tungsten.Utilities._

class LlvmCompatibilityPassTest {
  def convert(program: String): tungsten.Module = {
    val Left(module) = ModuleIO.readText(program, "<TEST>")
    LlvmCompatibilityPass(module)
  }

  @Test
  def mainType {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    return unit %r = ()\n" +
                  "  }\n" +
                  "}\n"
    val converted = convert(program)
    val mainType = converted.getFunction("main").returnType
    val retType = converted.get[tungsten.ReturnInstruction]("main.entry.r").get.value.ty
    assertEquals(tungsten.IntType(32), mainType)
    assertEquals(tungsten.IntType(32), retType)
  }
}
