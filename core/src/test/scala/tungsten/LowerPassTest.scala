package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class LowerPassTest {
  val pass = new LowerPass

  def testTransformation(expectedCode: String, module: Module, definitionNames: Symbol*) {
    val expectedModule = compileString(expectedCode)
    for (name <- definitionNames)
      assertEquals(expectedModule.definitions(name), module.definitions(name))
  }

  // @Test
  // def createVtableStruct {
  //   val program = "class @R { methods { %f, %g } }\n" +
  //                 "function unit @R.f(class @R %this)\n" +
  //                 "function unit @R.g(class @R %this)\n"
  //   val module = compileString(program)
  //   val clas = module.getClass("R")
  //   val expected = "struct @R._vtable_type {\n" +
  //                  "  field (class @R)->unit %f#0\n" +
  //                  "  field (class @R)->unit %g#1\n" +
  //                  "}\n"
  //   testTransformation(expected, pass.createVtableStruct(clas, module),
  //                      "R._vtable_type", "R._vtable_type.f#0", "R._vtable_type.g#1")
  // }

  // @Test
  // def createVtableGlobal {
  //   val program = "class @R { methods { %f, %g } }\n" +
  //                 "function unit @R.f(class @R %this)\n" +
  //                 "function unit @R.g(class @R %this)\n"
  //   val module = compileString(program)
  //   val clas = module.getClass("R")
  //   val expected = "global struct @R._vtable_type @R._vtable = \n" +
  //                  "  struct @R._vtable_type { (class @R)->unit @R.f, (class @R)->unit @R.g }\n"
  //   testTransformation(expected, pass.createVtableGlobal(clas, module), "R._vtable")
  // }

  @Test
  def replaceClassWithStruct {
    val program = "class @R { field int64 %x }\n"
    val module = compileString(program)
    val clas = module.getClass("R")
    val expected = "struct @R {\n" +
                   "  field struct @R._vtable_type* %_vtable_ptr\n" +
                   "  field int64 %x\n" +
                   "}\n"
    testTransformation(expected, pass.replaceClassWithStruct(clas, module))
  }
}
