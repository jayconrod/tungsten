/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

import scala.collection.immutable.TreeMap
import org.junit.Test
import org.junit.Assert._
import Utilities._
import TestUtilities._

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

    val expectedITableGlobalCode = "global [2 x struct @tungsten.itable_entry] @C.itable$ =\n" +
                                   "  [2 x struct @tungsten.itable_entry] {\n" +
                                   "    struct @tungsten.itable_entry {\n" +
                                   "      struct @tungsten.interface_info* @I.info$,\n" +
                                   "      bitcast struct @I.vtable_type$* @C.ivtable$.I to int8*\n" +
                                   "    },\n" +
                                   "    struct @tungsten.itable_entry {\n" +
                                   "      struct @tungsten.interface_info* @J.info$,\n" +
                                   "      bitcast struct @I.vtable_type$* @C.ivtable$.I to int8*\n" +
                                   "    }\n" +
                                   "  }\n"
    val expectedITableGlobal = compileString(expectedITableGlobalCode).getGlobal("C.itable$")
    val itableGlobal = pass.createITableGlobal(clas, ivtableMap, module).getGlobal("C.itable$")
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

    val expectedIVTableGlobalCode = "global struct @J.vtable_type$ @C.ivtable$.J = struct @J.vtable_type$ {\n" +
                                    "  bitcast (class @C)->unit @C.f to (interface @I)->unit\n" +
                                    "}\n"
    val expectedIVTableGlobal = compileString(expectedIVTableGlobalCode).getGlobal("C.ivtable$.J")
    val moduleWithIVTables = pass.createIVTableGlobals(clas, ivtableMap, module)
    val ivtableGlobal = moduleWithIVTables.getGlobal("C.ivtable$.J")
    assertEquals(expectedIVTableGlobal, ivtableGlobal)
    assertFalse(moduleWithIVTables.definitions.contains("C.ivtable$.I"))
  }

  @Test
  def createVTableStruct {
    val program = "class @R { methods { %f, %g } }\n" +
                  "function unit @R.f(class @R %this)\n" +
                  "function unit @R.g(class @R %this)\n"
    var module = compileString(program)
    val R = module.getClass("R")
    val ivtableMap = R.getIVTables(module)
    module = pass.addDefinitions(module)
    module = pass.createITableGlobal(R, ivtableMap, module)
    module = pass.createVTableStruct(R, module)
    
    val expectedCode = "struct @R.vtable_type$ {\n" +
                       "  field struct @tungsten.class_info* %info$\n" +
                       "  field struct @tungsten.array %itable$\n" +
                       "  field (class @R)->unit %f#0\n" +
                       "  field (class @R)->unit %g#1\n" +
                       "}\n"
    val expectedModule = compileString(expectedCode)
    val expected = expectedModule.getStruct("R.vtable_type$")
    val vtableStruct = module.getStruct("R.vtable_type$")
    assertEquals(expected, vtableStruct)

    val expectedFields = expectedModule.getFields(expected.fields)
    val vtableFields = module.getFields(vtableStruct.fields)
    (expectedFields zip vtableFields) foreach { case (expectedField, field) =>
      assertEquals(expectedField, field)
    }
  }

  @Test
  def createVtableGlobal {
    val program = "class @R { methods { %f, %g } }\n" +
                  "function unit @R.f(class @R %this)\n" +
                  "function unit @R.g(class @R %this)\n"
    val module = compileString(program)
    val R = module.getClass("R")
    val expectedCode = "global struct @R.vtable_type$ @R.vtable$ = struct @R.vtable_type$ {\n" +
                       "  struct @tungsten.class_info* @R.info$,\n" +
                       "  struct @tungsten.array {\n" +
                       "    bitcast [0 x struct @tungsten.itable_entry]* @R.itable$ to int8*,\n" +
                       "    int64 0\n" +
                       "  },\n" +
                       "  (class @R)->unit @R.f,\n" +
                       "  (class @R)->unit @R.g\n" +
                       "}\n"
    val expected = compileString(expectedCode).getGlobal("R.vtable$")
    val vtableGlobal = pass.createVTableGlobal(R, 0, module).getGlobal("R.vtable$")
    assertEquals(expected, vtableGlobal)
  }

  @Test
  def createClassStruct {
    val program = "class @R { field int64 %x }\n"
    val module = compileString(program)
    val R = module.getClass("R")
    val expectedCode = "struct @R.data$ {\n" +
                       "  field struct @R.vtable_type$* %vtable_ptr$\n" +
                       "  field int64 @R.x\n" +
                       "}\n"
    val expected = compileString(expectedCode).getStruct("R.data$")
    val Rstruct = pass.createClassStruct(R, module).getStruct("R.data$")
    assertEquals(expected, Rstruct)
  }

  @Test
  def removeFunctionTypeParameters {
    val program = "class @R\n" +
                  "function type %T @f[type %T](type %T %x)"
    val module = compileString(program)
    val f = pass.eliminateTypeParameters(module).getFunction("f")
    assertEquals(Nil, f.typeParameters)
  }

  @Test
  def lowerClassWithInterfaceAndParameter {
    val program = "class @R\n" +
                  "interface @I <: class @R {\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "function unit @I.f(interface @I %this)\n" +
                  "class @C[type %T] <: class @R {\n" +
                  "  interface @I { %f }\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "function unit @C.f[type %T](class @C[type %T] %this)\n"
    val module = compileString(program)
    val loweredModule = pass(module)

    val expectedProgram = "global struct @tungsten.interface_info @I.info$ = struct @tungsten.interface_info {\n" +
                          "  int32 0,\n" +
                          "  struct @tungsten.array {\n" +
                          "    bitcast [1 x int8]* @I.name$ to int8*,\n" +
                          "    int64 1\n" +
                          "  },\n" +
                          "  struct @tungsten.array {\n" +
                          "    bitcast null to int8*,\n" +
                          "    int64 0\n" +
                          "  },\n" +
                          "  struct @tungsten.class_info* @R.info$,\n" +
                          "  int64 1,\n" +
                          "  bitcast [1 x struct @tungsten.class_info*]* @I.supertype_info$ to struct @tungsten.class_info**,\n" +
                          "  bitcast null to int8*?*?*?\n" +
                          "}\n" +
                          "global struct @tungsten.class_info @C.info$ = struct @tungsten.class_info {\n" +
                          "  int32 1,\n" +
                          "  struct @tungsten.array {\n" +
                          "    bitcast [1 x int8]* @C.name$ to int8*,\n" +
                          "    int64 1\n" +
                          "  },\n" +
                          "  struct @tungsten.array {\n" +
                          "    bitcast [1 x struct @tungsten.type_parameter_info]* @C.type_parameter_info$ to int8*,\n" +
                          "    int64 1\n" +
                          "  },\n" +
                          "  bitcast struct @tungsten.class_info* @R.info$ to struct @tungsten.class_info*?,\n" +
                          "  int64 2,\n" +
                          "  bitcast [2 x struct @tungsten.class_info*]* @C.supertype_info$ to struct @tungsten.class_info**?,\n" +
                          "  bitcast null to int8*?*?*?,\n" +
                          "  int64 8\n" +
                          "}\n" +
                          "global [1 x int8] @I.name$ = \"I\"\n" +
                          "global [1 x int8] @C.name$ = \"C\"\n" +
                          "global [3 x int8] @C.type_parameter_names$#0 = \"C.T\"\n" +
                          "global [1 x struct @tungsten.type_parameter_info] @C.type_parameter_info$ = [1 x struct @tungsten.type_parameter_info] {\n" +
                          "  struct @tungsten.type_parameter_info {\n" +
                          "    int32 0,\n" +
                          "    struct @tungsten.array {\n" +
                          "      bitcast [3 x int8]* @C.type_parameter_names$#0 to int8*,\n" +
                          "      int64 3\n" +
                          "    }\n" +
                          "  }\n" +
                          "}\n" +
                          "global [1 x struct @tungsten.class_info*] @I.supertype_info$ = [1 x struct @tungsten.class_info*] {\n" +
                          "  struct @tungsten.class_info* @R.info$\n" +
                          "}\n" +
                          "global [2 x struct @tungsten.class_info*] @C.supertype_info$ = [2 x struct @tungsten.class_info*] {\n" +
                          "  struct @tungsten.class_info* @R.info$,\n" +
                          "  bitcast struct @tungsten.interface_info* @I.info$ to struct @tungsten.class_info*\n" +
                          "}\n" +
                          "struct @I.vtable_type$ {\n" +
                          "  field (struct @R.data$*)->unit %f#0\n" +
                          "}\n" +
                          "global struct @I.vtable_type$ @C.ivtable$.I = struct @I.vtable_type$ {\n" +
                          "  bitcast (struct @C.data$*)->unit @C.f to (struct @R.data$*)->unit\n" +
                          "}\n" +
                          "struct @C.vtable_type$ {\n" +
                          "  field struct @tungsten.class_info* %info$\n" +
                          "  field struct @tungsten.array %itable$\n" +
                          "  field (struct @C.data$*)->unit %f#0\n" +
                          "}\n" +
                          "global [1 x struct @tungsten.itable_entry] @C.itable$ = [1 x struct @tungsten.itable_entry] {\n" +
                          "  struct @tungsten.itable_entry {\n" +
                          "    struct @tungsten.interface_info* @I.info$,\n" +
                          "    bitcast struct @I.vtable_type$* @C.ivtable$.I to int8*\n" +
                          "  }\n" +
                          "}\n" +
                          "global struct @C.vtable_type$ @C.vtable$ = struct @C.vtable_type$ {\n" +
                          "  struct @tungsten.class_info* @C.info$,\n" +
                          "  struct @tungsten.array {\n" +
                          "    bitcast [1 x struct @tungsten.itable_entry]* @C.itable$ to int8*,\n" +
                          "    int64 1\n" +
                          "  },\n" +
                          "  (struct @C.data$*)->unit @C.f\n" +
                          "}\n" +
                          "struct @C.data$ {\n" +
                          "  field struct @C.vtable_type$* %vtable_ptr$\n" +
                          "}\n"
    val expectedModule = pass.addDefinitions(compileString(expectedProgram))
    assertModuleEqualsIgnoreSymbols(expectedModule, loweredModule)
  }

  @Test
  def lowerClassTypeInstructions {
    val program = "class @R\n" +
                  "interface @I[type %T] <: class @R\n" +
                  "class @C[type %T] <: class @R {\n" +
                  "  interface @I[class @C[type %T]]\n" +
                  "}\n"
    val module = compileString(program)
    val loweredModule = pass(module)

    val expectedProgram = "global struct @tungsten.class_info @C.info$ = struct @tungsten.class_info {\n" +
                          "  int32 1,\n" +
                          "  struct @tungsten.array {\n" +
                          "    bitcast [1 x int8]* @C.name$ to int8*,\n" +
                          "    int64 1\n" +
                          "  },\n" +
                          "  struct @tungsten.array {\n" +
                          "    bitcast [1 x struct @tungsten.type_parameter_info]* @C.type_parameter_info$ to int8*,\n" +
                          "    int64 1\n" +
                          "  },\n" +
                          "  bitcast struct @tungsten.class_info* @R.info$ to struct @tungsten.class_info*?,\n" +
                          "  int64 2,\n" +
                          "  bitcast [2 x struct @tungsten.class_info*]* @C.supertype_info$ to struct @tungsten.class_info**?,\n" +
                          "  bitcast [2 x int8*?*?]* @C.supertype_instruction_arrays$ to int8*?*?*?,\n" +
                          "  int64 8\n" +
                          "}\n" +
                          "global [1 x int8] @C.name$ = \"C\"\n" +
                          "global [1 x struct @tungsten.type_parameter_info] @C.type_parameter_info$ = [1 x struct @tungsten.type_parameter_info] {\n" +
                          "  struct @tungsten.type_parameter_info {\n" +
                          "    int32 0,\n" +
                          "    struct @tungsten.array {\n" +
                          "      bitcast [3 x int8]* @C.type_parameter_names$ to int8*,\n" +
                          "      int64 3\n" +
                          "    }\n" +
                          "  }\n" +
                          "}\n" +
                          "global [3 x int8] @C.type_parameter_names$ = \"C.T\"\n" +
                          "global [2 x struct @tungsten.class_info*] @C.supertype_info$ = [2 x struct @tungsten.class_info*] {\n" +
                          "  struct @tungsten.class_info* @R.info$,\n" +
                          "  bitcast struct @tungsten.interface_info* @I.info$ to struct @tungsten.class_info*\n" +
                          "}\n" +
                          "global [2 x int8*?*?] @C.supertype_instruction_arrays$ = [2 x int8*?*?] {\n" +
                          "  bitcast null to int8*?*?,\n" +
                          "  bitcast [3 x int8*?]* @C.supertype_instructions$#1 to int8*?*?\n" +
                          "}\n" +
                          "global [3 x int8*?] @C.supertype_instructions$#1 = [3 x int8*?] {\n" +
                          "  bitcast struct @tungsten.class_info* @C.info$ to int8*?,\n" +
                          "  bitcast int64 1 to int8*?,\n" +
                          "  bitcast int64 -1 to int8*?\n" +
                          "}\n"
    val expectedModule = compileString(expectedProgram)
    assertModuleEqualsIgnoreSymbols(expectedModule, loweredModule)
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
                        "  methods { @R.get, @I.f, %id }\n" +
                        "  field int64 %x\n" +
                        "}\n" +
                        "function unit @R.ctor(class @R %this, int64 %x)\n" +
                        "function int64 @R.get(class @R %this)\n" +
                        "function unit @I.f(interface @I %this)\n" +
                        "function unit @C.ctor(class @C %this)\n" +
                        "function type %T @C.id[type %T](class @C %this, type %T %x)\n" +
                        "function type %T @g[type %T](type %T %x)\n" +
                        "function unit @f {\n" +
                        "  block %entry {\n"
  val pass = new LowerPass
  val instPass = new LowerInstructionsPass

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
    var m = makeModule(code)
    m = pass.convertClassesAndInterfaces(m)
    m = instPass.processModule(m)
    val newInstructions = getInstructions(m)
    val expectedModule = makeModule(expectedCode)
    val expectedEntry = expectedModule.getBlock("f.entry")
    val expectedInstructions = expectedModule.getInstructions(expectedEntry.instructions)
    assertEquals(expectedInstructions.size, newInstructions.size)
    (newInstructions zip expectedInstructions) foreach { ind =>
      assertDefnEqualsIgnoreSymbols(ind._1, ind._2)
    }
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
    val expectedCode = "int8* %r.new$ = heaparray int64 16\n" +
                       "struct @R.data$* %r = bitcast int8* %r.new$\n" +
                       "unit %r.new$#1 = storeelement struct @R.vtable_type$* @R.vtable$, struct @R.data$* %r, int64 0, int64 0\n" +
                       "struct @tungsten.class_info** %r.new$#2 = bitcast int8* %r.new$\n" +
                       "unit %r.new$#3 = scall @R.ctor(struct @R.data$* %r, int64 2)\n"
    testConversion(expectedCode, code)
  }

  @Test
  def testComplexNew {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B <: class @R\n" +
                  "class @C[type %T] <: class @R\n" +
                  "class @D[type %T] <: class @R\n" +
                  "class @E[type %S, type %T] <: class @R {\n" +
                  "  constructors { %ctor }\n" +
                  "  field int64 %x\n" +
                  "}\n" +
                  "function unit @E.ctor[type %S, type %T](class @E[type %S, type %T] %this)\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @E[class @C[class @A], class @D[class @B]] %e = new @E.ctor()\n" +
                  "  }\n" +
                  "}\n"
    val expectedCode = "int8* %e.new$ = heaparray int64 48\n" +
                       "struct @E.data$* %e = bitcast int8* %e.new$\n" +
                       "unit %e.new$#1 = storeelement struct @E.vtable_type$* @E.vtable$, struct @E.data$* %e, int64 0, int64 0\n" +
                       "struct @tungsten.class_info** %e.new$#2 = bitcast int8* %e.new$\n" +
                       "unit %e.new$#3 = storeelement struct @tungsten.class_info* @C.info$, struct @tungsten.class_info** %e.new$#2, int64 2\n" +
                       "unit %e.new$#4 = storeelement struct @tungsten.class_info* @A.info$, struct @tungsten.class_info** %e.new$#2, int64 3\n" +
                       "unit %e.new$#5 = storeelement struct @tungsten.class_info* @D.info$, struct @tungsten.class_info** %e.new$#2, int64 4\n" +
                       "unit %e.new$#6 = storeelement struct @tungsten.class_info* @B.info$, struct @tungsten.class_info** %e.new$#2, int64 5\n" +
                       "unit %e.new$#7 = scall @E.ctor(struct @E.data$* %e)\n"
    var m = compileString(program)
    m = pass.convertClassesAndInterfaces(m)
    m = instPass.processModule(m)
    val newInstructions = getInstructions(m)
    val expectedModule = makeModule(expectedCode)
    val expectedInstructions = expectedModule.getInstructions(expectedModule.getBlock("f.entry").instructions)
    (newInstructions zip expectedInstructions) foreach { ind =>
      val (actual, expected) = ind
      assertDefnEqualsIgnoreSymbols(expected, actual)
    }
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
    val expectedCode = "struct @R.vtable_type$* %x.vtable$#1 = loadelement class @R %r, int64 0, int64 0\n" +
                       "(class @R)->int64 %x.method$#2 = loadelement struct @R.vtable_type$* %x.vtable$#1, int64 0, int64 2\n" +
                       "int64 %x = pcall (class @R)->int64 %x.method$#2(class @R %r)"
    testConversion(expectedCode, code)
  }

  @Test
  def testVCallInterface {
    val code = "interface @I %i = upcast null\n" +
               "unit %x = vcall interface @I %i:1()"
    val expectedCode = "interface @I %i = bitcast null\n" +
                       "int8* %x.vtable$#1 = scall @tungsten.load_ivtable(interface @I %i, struct @tungsten.interface_info* @I.info$)\n" +
                       "struct @I.vtable_type$* %x.vtable$#2 = bitcast int8* %x.vtable$#1\n" +
                       "(interface @I)->unit %x.method$#3 = loadelement struct @I.vtable_type$* %x.vtable$#2, int64 0, int64 1\n" +
                       "unit %x = pcall (interface @I)->unit %x.method$#3(interface @I %i)"
    testConversion(expectedCode, code)
  }

  @Test
  def testVLookupClass {
    val code = "(class @R)->int64 %m = vlookup class @R %r:0"
    val expectedCode = "struct @R.vtable_type$* %m.vtable$#1 = loadelement class @R %r, int64 0, int64 0\n" +
                       "(class @R)->int64 %m = loadelement struct @R.vtable_type$* %m.vtable$#1, int64 0, int64 2\n"
    testConversion(expectedCode, code)
  }

  @Test
  def testVLookupInterface {
    val code = "interface @I %i = upcast null\n" +
               "(interface @I)->unit %m = vlookup interface @I %i:1"
    val expectedCode = "interface @I %i = bitcast null\n" +
                       "int8* %m.vtable$#1 = scall @tungsten.load_ivtable(interface @I %i, struct @tungsten.interface_info* @I.info$)\n" +
                       "struct @I.vtable_type$* %m.vtable$#2 = bitcast int8* %m.vtable$#1\n" +
                       "(interface @I)->unit %m = loadelement struct @I.vtable_type$* %m.vtable$#2, int64 0, int64 1\n"
    testConversion(expectedCode, code)
  }                       

  @Test
  def testPCallTypeParameters {
    val code = "class @C %x = upcast null\n" +
               "class @C %y = pcall [@g.T](type @g.T)->type @g.T @g[class @C](class @C %x)\n"
    val expectedCode = "class @C %x = bitcast null\n" +
                       "type @g.T %y.cast$#1 = bitcast class @C %x\n" +
                       "type @g.T %y.cast$#2 = pcall [@g.T](type @g.T)->type @g.T @g(type @g.T %y.cast$#1)\n" +
                       "class @C %y = bitcast type @g.T %y.cast$#2"
    testConversion(expectedCode, code)
  }

  @Test
  def testSCallTypeParameters {
    val code = "class @C %x = upcast null\n" +
               "class @C %y = scall @g[class @C](class @C %x)\n"
    val expectedCode = "class @C %x = bitcast null\n" +
                       "type @g.T %y.cast$#1 = bitcast class @C %x\n" +
                       "type @g.T %y.cast$#2 = scall @g(type @g.T %y.cast$#1)\n" +
                       "class @C %y = bitcast type @g.T %y.cast$#2\n"
    testConversion(expectedCode, code)
  }

  @Test
  def testVCallTypeParameters {
    val code = "class @C %x = upcast null\n" +
               "class @C %y = vcall class @C %x:2[class @C](class @C %x)\n"
    val expectedCode = "class @C %x = bitcast null\n" +
                       "struct @C.vtable_type$* %y.vtable$#1 = loadelement class @C %x, int64 0, int64 0\n" +
                       "[@C.id.T](class @C, type @C.id.T)->type @C.id.T %y.method$#2 = loadelement struct @C.vtable_type$* %y.vtable$#1, int64 0, int64 4\n" +
                       "type @C.id.T %y.cast$#3 = bitcast class @C %x\n" +
                       "type @C.id.T %y.cast$#4 = pcall [@C.id.T](class @C, type @C.id.T)->type @C.id.T %y.method$#2(class @C %x, type @C.id.T %y.cast$#3)\n" +
                       "class @C %y = bitcast type @C.id.T %y.cast$#4\n"
    testConversion(expectedCode, code)
  }

  @Test
  def testNullCheck {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.bb(int64 12)\n" +
                  "  }\n" +
                  "  block %bb(int64 %m) {\n" +
                  "    class? @tungsten.Object %a = upcast null\n" +
                  "    int64 %n = binop int64 %m + int64 %m\n" +
                  "    class @tungsten.Object %b = nullcheck class? @tungsten.Object %a\n" +
                  "    branch @f.exit(int64 %n, class @tungsten.Object %b)\n" +
                  "  } catch @f.cb(int64 %m)\n" +
                  "  block %exit(int64 %n, class @tungsten.Object %b) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "  block %cb(int64 %m) {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    val module = linkRuntime(compileString(program))
    val convertedModule = pass(module)
    
    val expectedProgram = "function unit @f {\n" +
                          "  block %entry {\n" +
                          "    unit %anon$#1 = branch @f.bb(int64 12)\n" +
                          "  }\n" +
                          "  block %bb(int64 @f.bb.m) {\n" +
                          "    struct @tungsten.Object.data$*? %a = bitcast null\n" +
                          "    int64 %n = binop int64 %m + int64 %m\n" +
                          "    struct @tungsten.Object.data$* %b = bitcast struct @tungsten.Object.data$*? %a\n" +
                          "    boolean @f.bb.b.nullcheck$#16 = relop struct @tungsten.Object.data$*? %a == bitcast null to struct @tungsten.Object.data$*?\n" +
                          "    unit @f.bb.b.nullcheck$#17 = cond boolean @f.bb.b.nullcheck$#16 ? @f.bb.b.nullcheck$.error$#14(int64 %m) : @f.bb#6(struct @tungsten.Object.data$* %b, int64 %n, int64 %m)\n" +
                          "  } catch @f.cb(int64 %m)\n" +
                          "  block %bb#6(struct @tungsten.Object.data$* @f.bb.b#9, int64 @f.bb.n#8, int64 @f.bb.m#7) {\n" +
                          "    unit %anon$#2 = branch @f.exit(int64 %n#8, struct @tungsten.Object.data$* %b#9)\n" +
                          "  } catch @f.cb(int64 %m#7)\n" +
                          "  block @f.bb.b.nullcheck$.error$#14(int64 @f.bb.m#15) {\n" +
                          "    int8* @f.bb.b.nullcheck$.new$#10 = heaparray int64 8\n" +
                          "    struct @tungsten.NullPointerException.data$* @f.bb.b.nullcheck$#11 = bitcast int8* @f.bb.b.nullcheck$.new$#10\n" +
                          "    unit @f.bb.b.nullcheck$.new$#12 = storeelement struct @tungsten.NullPointerException.vtable_type$* @tungsten.NullPointerException.vtable$, struct @tungsten.NullPointerException.data$* @f.bb.b.nullcheck$#11, int64 0, int64 0\n" +
                          "    struct @tungsten.class_info** @f.bb.b.nullcheck$.new$#13 = bitcast int8* @f.bb.b.nullcheck$.new$#10\n" +
                          "    unit @f.bb.b.nullcheck$.new$#14 = scall @tungsten.NullPointerException.ctor(struct @tungsten.NullPointerException.data$* @f.bb.b.nullcheck$#11)\n" +
                          "    unit @f.bb.b.nullcheck$#15 = throw struct @tungsten.NullPointerException.data$* @f.bb.b.nullcheck$#11\n" +
                          "  } catch @f.cb(int64 @f.bb.m#16)\n" +
                          "  block %exit(int64 @f.exit.n, struct @tungsten.Object.data$* @f.exit.b) {\n" +
                          "    unit %anon$#3 = return ()\n" +
                          "  }\n" +
                          "  block %cb(int64 @f.cb.m) {\n" +
                          "    struct @tungsten.Exception.data$* %exn = catch \n" +
                          "    unit %anon$#4 = return ()\n" +
                          "  }\n" +
                          "}"

    val expectedModule = compileString(expectedProgram)
    assertModuleEqualsIgnoreSymbols(expectedModule, convertedModule)
  }

  @Test
  def testInstanceOfClassInst {
    val code = "boolean %isa = instanceof class @R %r: class @C\n"
    val expectedCode = "struct @R.data$* %isa#1 = bitcast class @R %r\n" +
                       "boolean %isa = scall @tungsten.instanceof(struct @R.data$* %isa#1, struct @tungsten.class_info* @C.info$, bitcast null to struct @tungsten.class_info**?)"
    testConversion(expectedCode, code)
  }

  @Test
  def testInstanceOfInterfaceInst {
    val code = "boolean %isa = instanceof class @R %r: interface @I"
    val expectedCode = "struct @R.data$* %isa#1 = bitcast class @R %r\n" +
                       "boolean %isa = scall @tungsten.instanceof(struct @R.data$* %isa#1,\n" +
                       "                                          bitcast struct @tungsten.interface_info* @I.info$ to struct @tungsten.class_info*,\n" +
                       "                                          bitcast null to struct @tungsten.class_info**?)\n"
    testConversion(expectedCode, code)
  }

  @Test
  def testInstanceofComplexInst {
    val code = "boolean %isa = instanceof class @R %r: class @C[class @R]"
    val expectedCode = "struct @R.data$* %isa#1 = bitcast class @R %r\n" +
                       "struct @tungsten.class_info** %isa#2 = stackarray int64 1\n" +
                       "unit %isa#3 = storeelement struct @tungsten.class_info* @R.info$, struct @tungsten.class_info** %isa#2,int64 0\n" +
                       "boolean %isa = scall @tungsten.instanceof(struct @R.data$* %isa#1,\n" +
                       "                                          struct @tungsten.class_info* @C.info$,\n" +
                       "                                          bitcast struct @tungsten.class_info** %isa#2 to struct @tungsten.class_info**?)\n"
    testConversion(expectedCode, code)
  }

  @Test
  def testCheckedCastInst {
    val program = "class @C[type %T] <: class @tungsten.Object\n" +
                  "function class @C[class @tungsten.Object] @f {\n" +
                  "  block %entry {\n" +
                  "    class @tungsten.Object %x = bitcast null\n" +
                  "    class @C[class @tungsten.Object] %y = checkedcast class @tungsten.Object %x\n" +
                  "    unit %ret = return class @C[class @tungsten.Object] %y\n" +
                  "  }\n" +
                  "}\n"
    val module = linkRuntime(compileString(program))
    val loweredModule = pass(module)
    assertTrue(loweredModule.definitions.contains("f"))

    val expectedProgram = "function struct @C.data$* @f {\n" +
                          "  block %entry {\n" +
                          "    struct @tungsten.Object.data$* %x = bitcast null\n" +
                          "    struct @tungsten.Object.data$* %y.cast$#1 = bitcast struct @tungsten.Object.data$* %x\n" +
                          "    struct @tungsten.class_info** %y.cast$#2 = stackarray int64 1\n" +
                          "    unit %y.cast$#3 = storeelement struct @tungsten.class_info* @tungsten.Object.info$, struct @tungsten.class_info** %y.cast$#2, int64 0\n" +
                          "    struct @tungsten.class_info**? %y.cast$#4 = bitcast struct @tungsten.class_info** %y.cast$#2\n" +
                          "    boolean %y.cast$#5 = scall @tungsten.instanceof(struct @tungsten.Object.data$* %y.cast$#1, struct @tungsten.class_info* @C.info$, struct @tungsten.class_info**? %y.cast$#4)\n" +
                          "    unit %y.cast$#6 = cond boolean %y.cast$#5 ? @f.entry#7(struct @tungsten.Object.data$* %x)\n" +
                          "                                              : @f.entry.y.cast$.error$#8()\n" +
                          "  }\n" +
                          "  block %entry#7(struct @tungsten.Object.data$* %x#14) {\n" +
                          "    struct @C.data$* %y = bitcast struct @tungsten.Object.data$* %x#14\n" +
                          "    unit %ret = return struct @C.data$* %y\n" +
                          "  }\n" +
                          "  block %entry.y.cast$.error$#8 {\n" +
                          "    int8* @f.entry.y.cast$.new$#9 = heaparray int64 8\n" +
                          "    struct @tungsten.CastException.data$* @f.entry.y.cast$#10 = bitcast int8* @f.entry.y.cast$.new$#9\n" +
                          "    unit @f.entry.y.cast$.new$#11 = storeelement struct @tungsten.CastException.vtable_type$* @tungsten.CastException.vtable$, struct @tungsten.CastException.data$* @f.entry.y.cast$#10, int64 0, int64 0\n" +
                          "    struct @tungsten.class_info** @f.entry.y.cast$.new$#12 = bitcast struct @tungsten.CastException.data$* @f.entry.y.cast$#10\n" +
                          "    unit @f.entry.y.cast$.new$#13 = scall @tungsten.CastException.ctor(struct @tungsten.CastException.data$* @f.entry.y.cast$#10)\n" +
                          "    unit @f.entry.y.cast$ = throw struct @tungsten.CastException.data$* @f.entry.y.cast$#10\n" +
                          "  }\n" +
                          "}\n"
    val expectedModule = compileString(expectedProgram)
    assertTrue(expectedModule.definitions.contains("f"))
    assertModuleEqualsIgnoreSymbols(expectedModule, loweredModule)
  }


  @Test
  def substituteClassType {
    val module = makeModule("return ()")
    val ty = ClassType("C")
    val expectedTy = PointerType(StructType("C.data$"))
    assertEquals(expectedTy, pass.substituteType(ty, Map(), module))
  }

  @Test
  def substituteInterfaceType {
    val module = makeModule("return ()")
    val ty = InterfaceType("I")
    val expectedTy = PointerType(StructType("R.data$"))
    val interfaceBaseClassNames = Map((symbolFromString("I"), symbolFromString("R")))
    assertEquals(expectedTy, pass.substituteType(ty, interfaceBaseClassNames, module))
  }

  @Test
  def substituteVariableType {
    val module = makeModule("return ()")
    val ty = VariableType("g.T")
    val expectedTy = PointerType(StructType("R.data$"))
    assertEquals(expectedTy, pass.substituteType(ty, Map(), module))
  }

  @Test
  def substituteFunctionType {
    val module = makeModule("return ()")
    val ty = FunctionType(VariableType("g.T"),
                          List("g.T"),
                          List(VariableType("g.T")))
    val expectedTy = FunctionType(VariableType("g.T"),
                                  Nil,
                                  List(VariableType("g.T")))
    assertEquals(expectedTy, pass.substituteType(ty, Map(), module))
  }
}
