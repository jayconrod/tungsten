package tungsten

import scala.collection.immutable.TreeMap
import org.junit.{Test, Ignore}
import org.junit.Assert._
import Utilities._

class LowerPassTest {
  val pass = new LowerPass

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
                                    "  bitcast [2 x struct @tungsten._itable_entry]* @C._itable to struct @tungsten._itable_entry*,\n" +
                                    "  int64 2,\n" +
                                    "  bitcast (class @C)->unit @C.f to (interface @I)->unit\n" +
                                    "}\n"
    val expectedIVTableGlobal = compileString(expectedIVTableGlobalCode).getGlobal("C._ivtable.J")
    val moduleWithIVTables = pass.createIVTableGlobals(clas, ivtableMap, module)
    val ivtableGlobal = moduleWithIVTables.getGlobal("C._ivtable.J")
    assertEquals(expectedIVTableGlobal, ivtableGlobal)
    assertFalse(moduleWithIVTables.definitions.contains("C._ivtable.I"))
  }

  @Test
  def createVTableStruct {
    val program = "class @R { methods { %f, %g } }\n" +
                  "function unit @R.f(class @R %this)\n" +
                  "function unit @R.g(class @R %this)\n"
    var module = compileString(program)
    val R = module.getClass("R")
    val ivtableMap = R.getIVTables(module)
    module = pass.createITableEntryStruct(module)
    module = pass.createITableGlobal(R, ivtableMap, module)
    
    val expectedCode = "struct @R._vtable_type {\n" +
                       "  field struct @tungsten._itable_entry* %_itable_ptr\n" +
                       "  field int64 %_itable_size\n" +
                       "  field (class @R)->unit %f#0\n" +
                       "  field (class @R)->unit %g#1\n" +
                       "}\n"
    val expected = compileString(expectedCode).getStruct("R._vtable_type")
    val vtableStruct = pass.createVTableStruct(R, module).getStruct("R._vtable_type")
    assertEquals(expected, vtableStruct)
  }

  @Test
  def createVtableGlobal {
    val program = "class @R { methods { %f, %g } }\n" +
                  "function unit @R.f(class @R %this)\n" +
                  "function unit @R.g(class @R %this)\n"
    val module = compileString(program)
    val R = module.getClass("R")
    val expectedCode = "global struct @R._vtable_type @R._vtable = struct @R._vtable_type {\n" +
                       "  bitcast [0 x struct @tungsten._itable_entry]* @R._itable to struct @tungsten._itable_entry*,\n" +
                       "  int64 0,\n" +
                       "  (class @R)->unit @R.f,\n" +
                       "  (class @R)->unit @R.g\n" +
                       "}\n"
    val expected = compileString(expectedCode).getGlobal("R._vtable")
    val vtableGlobal = pass.createVTableGlobal(R, 0, module).getGlobal("R._vtable")
    assertEquals(expected, vtableGlobal)
  }

  @Test
  def replaceClassWithStruct {
    val program = "class @R { field int64 %x }\n"
    val module = compileString(program)
    val R = module.getClass("R")
    val expectedCode = "struct @R {\n" +
                       "  field struct @R._vtable_type* %_vtable_ptr\n" +
                       "  field int64 %x\n" +
                       "}\n"
    val expected = compileString(expectedCode).getStruct("R")
    val Rstruct = pass.replaceClassWithStruct(R, module).getStruct("R")
    assertEquals(expected, Rstruct)
  }
}


class LowerPassInstructionConversionTest {
  val programTemplate = "class @R {\n" +
                        "  constructors { %ctor }\n" +
                        "  methods { %get }\n" +
                        "  field int64 %x\n" +
                        "}\n" +
                        "interface @I <: class @R {\n" +
                        "  methods { @R.get, %f }\n" +
                        "}\n" +
                        "class @C <: class @R {\n" +
                        "  interface @I { @R.get, @I.f }\n" +
                        "  constructors { %ctor }\n" +
                        "  methods { @R.get, @I.f }\n" +
                        "  field int64 %x\n" +
                        "}\n" +
                        "function unit @R.ctor(class @R %this, int64 %x)\n" +
                        "function int64 @R.get(class @R %this)\n" +
                        "function unit @I.f(interface @I %this)\n" +
                        "function unit @C.ctor(class @C %this)\n" +
                        "function unit @f {\n" +
                        "  block %entry {\n"
  val pass = new LowerPass

  def makeModule(code: String): Module = {
    val program = programTemplate + code + "} }"
    compileString(program)
  }

  def getInstructions(module: Module): List[Instruction] = {
    val block = module.getBlock("f.entry")
    module.getInstructions(block.instructions)
  }

  def makeInstructions(code: String): List[Instruction] = {
    val module = makeModule(code)
    getInstructions(module)
  }

  def testConversion(expectedCode: String, code: String)
  {
    val module = makeModule(code)
    var processedModule = pass.convertClassesAndInterfaces(module)
    processedModule = pass.convertInstructions(processedModule)
    val instructions = getInstructions(processedModule)
    val expectedInstructions = makeInstructions(expectedCode)
    assertEquals(expectedInstructions, instructions)
  }

  @Test
  def testAddress {
    val code = "int64* %x = address class @R %r, int64 0"
    val expectedCode = "int64* %x = address class @R %r, int64 0, int64 1"
    testConversion(expectedCode, code)
  }

  @Test
  def testLoadElement {
    val code = "int64 %x = loadelement class @R %r, int64 0"
    val expectedCode = "int64 %x = loadelement class @R %r, int64 0, int64 1"
    testConversion(expectedCode, code)
  }

  @Test
  def testNew {
    val code = "class @R %r = new @R.ctor(int64 2)"
    val expectedCode = "struct @R* %r = heap\n" +
                       "unit %r._init#1 = scall @R.ctor(struct @R* %r, int64 2)"
    testConversion(expectedCode, code)
  }

  @Test
  def testStoreElement {
    val code = "storeelement int64 42, class @R %r, int64 0"
    val expectedCode = "storeelement int64 42, class @R %r, int64 0, int64 1"
    testConversion(expectedCode, code)
  }

  @Test
  def testNonClassElementPreserved {
    val code = "[2 x int64]* %a = stack\n" +
               "int64* %b = address [2 x int64]* %a, int64 0, int64 1"
    testConversion(code, code)
  }

  @Test
  def testVCallClass {
    val code = "int64 %x = vcall class @R %r:0()"
    val expectedCode = "struct @R._vtable_type* %x._vtable#1 = loadelement class @R %r, int64 0, int64 0\n" +
                       "(class @R)->int64 %x._method#2 = loadelement struct @R._vtable_type* %x._vtable#1, int64 0, int64 2\n" +
                       "int64 %x = pcall (class @R)->int64 %x._method#2(class @R %r)"
    testConversion(expectedCode, code)
  }

  @Test
  def testVCallInterface {
    val code = "interface @I %i = upcast null\n" +
               "unit %x = vcall interface @I %i:1()"
    val expectedCode = "interface @I %i = upcast null\n" +
                       "struct @I._vtable_type* %x._vtable#1 = scall @tungsten.load_ivtable(interface @I %i, \"I\")\n" +
                       "(interface @I)->unit %x._method#2 = loadelement struct @I._vtable_type* %x._vtable#1, int64 0, int64 3\n" +
                       "unit %x = pcall (interface @I)->unit %x._method#2(interface @I %i)"
    testConversion(expectedCode, code)
  }
}
