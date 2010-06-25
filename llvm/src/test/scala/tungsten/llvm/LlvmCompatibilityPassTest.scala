package tungsten.llvm

import org.junit.Test
import org.junit.Assert._
import tungsten.{ModuleIO, Symbol}
import tungsten.Utilities._

class LlvmCompatibilityPassTest {
  val pass = new LlvmCompatibilityPass
  val codeTemplate = "is64bit: true\n" +
                     "function unit @main {\n" +
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
    val expectedProgram = "annotation @tungsten.NoReturn\n" +
                          "function int8* @tungsten.malloc(int32 %size)\n" +
                          "@tungsten.NoReturn function unit @tungsten.exit(int32 %code)\n"
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
  def convertIndexTo32BitConstant {
    val sibling = Symbol("foo")
    assertEquals((tungsten.IntValue(0, 32), None),
                 pass.convertWordTo32Bit(tungsten.IntValue(0, 64), sibling))
  }

  @Test
  def convertIndexTo32BitVar {
    val sibling = Symbol("foo")
    val word = tungsten.DefinedValue("i", tungsten.IntType(64))
    val expected = tungsten.DefinedValue("llvmCompat#1", tungsten.IntType(32))
    val cast = tungsten.IntegerTruncateInstruction(expected.value, expected.ty, word)
    assertEquals((expected, Some(cast)), pass.convertWordTo32Bit(word, sibling))
  }

  @Test
  def addressInst {
    val code = "address int64* %a = int64* %base, int64 %b"
    val expected = "itruncate int32 %llvmCompat#1 = int64 %b\n" +
                   "address int64* %a = int64* %base, int32 %llvmCompat#1"
    testCode(expected, code)
  }

  @Test
  def extractInst {
    val code = "extract unit %a = [2 x unit] {(), ()}, int64 1"
    val expected = "extract unit %a = [2 x unit] {(), ()}, int32 1"
    testCode(expected, code)
  }

  @Test
  def heapInst {
    val code = "heap int64* %a"
    val expected = "scall int8* %llvmCompat#1 = @tungsten.malloc(int32 8)\n" +
                   "bitcast int64* %a = int8* %llvmCompat#1"
    testCode(expected, code)
  }

  @Test
  def heapArrayInst {
    val code = "heaparray int64* %a = int64 2"
    val expected = "binop int64 %llvmCompat#1 = int64 2 * int64 8\n" +
                   "scall int8* %llvmCompat#2 = @tungsten.malloc(int64 %llvmCompat#1)\n" +
                   "bitcast int64* %a = int8* %llvmCompat#2"
    testCode(expected, code)
  }

  @Test
  def insertInst {
    val code = "insert [2 x unit] %a = (), [2 x unit] {(), ()}, int64 1"
    val expected = "insert [2 x unit] %a = (), [2 x unit] {(), ()}, int32 1"
    testCode(expected, code)
  }

  @Test
  def intrinsicExitInst {
    val code = "intrinsic unit %x = exit(int32 1)"
    val expected = "scall unit %x = @tungsten.exit(int32 1)"
    testCode(expected, code)
  }

  @Test
  def stackArrayInst {
    val code = "stackarray int64* %a = int64 2"
    val expected = "stackarray int64* %a = int32 2"
    testCode(expected, code)
  }
}
