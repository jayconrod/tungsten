package tungsten

import scala.collection.immutable.TreeMap
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

  @Test
  def getIVTablesPrimary {
    val program = "class @R\n" +
                  "class @A <: class @R {\n" +
                  "  interface @I { %f }\n" +
                  "  methods { %f, %g }\n" +
                  "}\n" +
                  "class @B <: class @A {\n" +
                  "  interface @J { @A.g, %h }\n" +
                  "  methods { @A.f, @A.g, %h }\n" +
                  "}\n" +
                  "interface @I <: class @R {\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "interface @J <: interface @I {\n" +
                  "  methods { @I.f, %g }\n" +
                  "}\n" +
                  "function unit @A.f(class @A %this)\n" +
                  "function unit @A.g(class @A %this)\n" +
                  "function unit @B.h(class @B %this)\n" +
                  "function unit @I.f(interface @I %this)\n" +
                  "function unit @J.g(interface @J %this)\n"
    val module = compileString(program)
    val errors = module.validate
    assertTrue(errors.isEmpty)
    val B = module.getClass("B")
    val expectedIVTables = Map[Symbol, Either[List[Symbol], Symbol]](("I", Left(List("A.f"))),
                                                                     ("J", Left(List("A.g", "B.h"))))
    val ivtables = B.getIVTables(module)
    assertEquals(expectedIVTables, ivtables)
  }

  @Test
  def getIVTablesOverride {
    val program = "class @R\n" +
                  "class @C <: class @R {\n" +
                  "  interface @J\n" +
                  "}\n" +
                  "interface @J <: interface @I\n" +
                  "interface @I <: class @R\n"
    val module = compileString(program)
    val errors = module.validate
    assertTrue(errors.isEmpty)
    val C = module.getClass("C")
    val expectedIVTables = Map[Symbol, Either[List[Symbol], Symbol]](("J", Left(Nil)),
                                                                     ("I", Right("J")))
    val ivtables = C.getIVTables(module)
    assertEquals(expectedIVTables, ivtables)
  }

  @Test
  def createITableGlobal {
    val clas = Class("C", Nil, None,
                     List(InterfaceType("I"), InterfaceType("J")),
                     List(List("f"), List("f")),
                     Nil,
                     List("f"),
                     Nil)
    val definitions = TreeMap((clas.name -> clas))
    val module = new Module(definitions = definitions)
    val ivtableMap = Map[Symbol, Either[List[Symbol], Symbol]](("I", Left(List("f"))),
                                                               ("J", Right("I")))

    val expectedITableGlobalCode = "global [2 x struct @tungsten._itable_entry] @C._itable =\n" +
                                   "  [2 x struct @tungsten._itable_entry] {\n" +
                                   "    struct @tungsten._itable_entry {\n" +
                                   "      \"I\",\n" +
                                   "      bitcast struct @I._vtable_type* @C._ivtable.I to int8*\n" +
                                   "    },\n" +
                                   "    struct @tungsten._itable_entry {\n" +
                                   "      \"J\",\n" +
                                   "      bitcast struct @I._vtable_type* @C._ivtable.I to int8*\n" +
                                   "    }\n" +
                                   "  }\n"
    val expectedITableGlobal = compileString(expectedITableGlobalCode).getGlobal("C._itable")
    val itableGlobal = pass.createITableGlobal(clas, ivtableMap, module).getGlobal("C._itable")
    assertEquals(expectedITableGlobal, itableGlobal)
  }

  @Test
  def createIVTableGlobals {
    val program = "class @R\n" +
                  "class @C <: class @R {\n" +
                  "  interface @J { %f }\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "interface @I <: class @R {\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "interface @J <: interface @I {\n" +
                  "  methods { @I.f }\n" +
                  "}\n" +
                  "function unit @C.f(class @C %this)\n" +
                  "function unit @I.f(interface @I %this)\n"
    val module = compileString(program)
    val clas = module.getClass("C")
    val ivtableMap = clas.getIVTables(module)

    val expectedIVTableGlobalCode = "global struct @J._vtable_type @C._ivtable.J = struct @J._vtable_type {\n" +
                                    "  bitcast (class @C)->unit @C.f to (interface @I)->unit\n" +
                                    "}\n"
    val expectedIVTableGlobal = compileString(expectedIVTableGlobalCode).getGlobal("C._ivtable.J")
    val moduleWithIVTables = pass.createIVTableGlobals(clas, ivtableMap, module)
    val ivtableGlobal = moduleWithIVTables.getGlobal("C._ivtable.J")
    assertEquals(expectedIVTableGlobal, ivtableGlobal)
    assertFalse(moduleWithIVTables.definitions.contains("C._ivtable.I"))
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
