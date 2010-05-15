package tungsten.llvm

import org.junit.Test
import org.junit.Assert._

class InstructionTest {
  val module = new Module(None, None, Map())

  @Test
  def gepTyBasic {
    val gep = GetElementPointerInstruction("a", DefinedValue("b", PointerType(IntType(32))),
                                           List(IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test
  def gepTyArray {
    val gep = GetElementPointerInstruction("a", 
                                           DefinedValue("b", PointerType(ArrayType(2L, IntType(32)))),
                                           List(IntValue(0L, 32), IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test
  def gepAnonStructTy {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test
  def gepNamedStructTy {
    val struct = Struct("%A", List(IntType(32)))
    val module = new Module(None, None, Map((struct.name -> struct)))
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(NamedStructType("%A"))),
                                           List(IntValue(0L, 32), IntValue(0L, 32)))
    assertEquals(PointerType(IntType(32)), gep.ty(module))
  }

  @Test(expected=classOf[LlvmConversionException])
  def gepStructTyNonConstIndex {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), DefinedValue("c", IntType(32))))
    gep.ty(module)
  }

  @Test(expected=classOf[LlvmConversionException])
  def gepStructTyWeirdSizeIndex {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), IntValue(0L, 31)))
    gep.ty(module)
  }

  @Test(expected=classOf[LlvmConversionException])
  def gepStructTyIndexOutOfBounds {
    val gep = GetElementPointerInstruction("a",
                                           DefinedValue("b", PointerType(StructType(List(IntType(32))))),
                                           List(IntValue(0L, 32), IntValue(1L, 32)))
    gep.ty(module)
  }
}
