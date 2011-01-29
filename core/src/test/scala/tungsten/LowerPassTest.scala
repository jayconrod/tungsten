package tungsten

import scala.collection.immutable.TreeMap
import org.junit.Test
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

  @Test
  def convertNewInstruction {
    val program = "class @R {\n" +
                  "  constructors { %ctor }\n" +
                  "  field int64 %x\n" +
                  "}\n" +
                  "function unit @R.ctor(class @R %this, int64 %x)\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @R %r = new @R.ctor(int64 2)\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    var module = compileString(program)
    module = pass.convertClassesAndInterfaces(module)
    module = pass.convertInstructions(module)
    val block = module.getBlock("f.entry")
    val instructions = module.getInstructions(block.instructions)

    val expectedCode = "function unit @f {\n" +
                       "  block %entry {\n" +
                       "    struct @R* %r = heap\n" +
                       "    unit %r._init#1 = scall @R.ctor(struct @R* %r, int64 2)\n" +
                       "    return ()\n" +
                       "  }\n" +
                       "}\n"
    val expectedModule = compileString(expectedCode)
    val expectedBlock = expectedModule.getBlock("f.entry")
    val expectedInstructions = expectedModule.getInstructions(expectedBlock.instructions)
    assertEquals(expectedInstructions, instructions)
  }
}
