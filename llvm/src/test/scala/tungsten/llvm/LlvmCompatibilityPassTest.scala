package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import tungsten.ModuleIO
import tungsten.Utilities._

class LlvmCompatibilityPassTest {
  val pass = new LlvmCompatibilityPass
  val codeTemplate = "function unit @main {\n" +
                     "  block %%entry {\n" +
                     "    %s\n" +
                     "    return unit %%r = ()\n" +
                     "  }\n" +
                     "}\n"

  def convert(program: String): tungsten.Module = {
    val module = ModuleIO.readText(program, "<TEST>")
    LlvmCompatibilityPass(module)
  }

  def testCode(expected: String, code: String) {
    val module = ModuleIO.readText(codeTemplate.format(code))
    val block = module.getBlock("main.entry")
    val instructions = module.getInstructions(block.instructions)
    assertEquals(2, instructions.size)
    val instruction = instructions.head
    val converted = pass.convertInstruction(instruction, module)

    val expectedModule = ModuleIO.readText(codeTemplate.format(expected))
    val expectedBlock = expectedModule.getBlock("main.entry")
    val expectedInstructions = expectedModule.getInstructions(expectedBlock.instructions)

    assertEquals(expectedInstructions.take(expectedInstructions.size - 1), converted)
  }    

  @Test
  def runtimeFunctionsPresent {
    val module = new tungsten.Module
    val processed = pass.addRuntime(module)
    val expectedProgram = "function int8* @tungsten.malloc(int32 %size)\n"
    val expected = ModuleIO.readText(expectedProgram)
    assertEquals(expected.definitions, processed.definitions)
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

  @Test
  def heapInst {
    val code = "heap int64* %a"
    val expected = "scall int8* %llvmCompat#1 = @tungsten.malloc(int32 8)\n" +
                   "bitcast int64* %a = int8* %llvmCompat#1"
    testCode(expected, code)
  }
}
