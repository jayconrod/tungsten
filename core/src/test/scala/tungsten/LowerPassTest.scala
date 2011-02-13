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

    val expectedITableGlobalCode = "global [2 x struct @tungsten.itable_entry$] @C.itable$ =\n" +
                                   "  [2 x struct @tungsten.itable_entry$] {\n" +
                                   "    struct @tungsten.itable_entry$ {\n" +
                                   "      \"I\",\n" +
                                   "      bitcast struct @I.vtable_type$* @C.ivtable$.I to int8*\n" +
                                   "    },\n" +
                                   "    struct @tungsten.itable_entry$ {\n" +
                                   "      \"J\",\n" +
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
                                    "  bitcast [2 x struct @tungsten.itable_entry$]* @C.itable$ to struct @tungsten.itable_entry$*,\n" +
                                    "  int64 2,\n" +
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
    module = pass.createITableEntryStruct(module)
    module = pass.createITableGlobal(R, ivtableMap, module)
    module = pass.createVTableStruct(R, module)
    
    val expectedCode = "struct @R.vtable_type$ {\n" +
                       "  field struct @tungsten.itable_entry$* %itable_ptr$\n" +
                       "  field int64 %itable_size$\n" +
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
                       "  bitcast [0 x struct @tungsten.itable_entry$]* @R.itable$ to struct @tungsten.itable_entry$*,\n" +
                       "  int64 0,\n" +
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
    val f = pass.convertFunctions(module).getFunction("f")
    assertEquals(Nil, f.typeParameters)
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
    var processedModule = module
    processedModule = pass.convertClassesAndInterfaces(module)
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
    val expectedCode = "struct @R.data$* %r = heap\n" +
                       "unit %r.init$#1 = scall @R.ctor(struct @R.data$* %r, int64 2)"
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
    val expectedCode = "struct @R.vtable_type$* %x.vtable$#1 = loadelement class @R %r, int64 0, int64 0\n" +
                       "(class @R)->int64 %x.method$#2 = loadelement struct @R.vtable_type$* %x.vtable$#1, int64 0, int64 2\n" +
                       "int64 %x = pcall (class @R)->int64 %x.method$#2(class @R %r)"
    testConversion(expectedCode, code)
  }

  @Test
  def testVCallInterface {
    val code = "interface @I %i = upcast null\n" +
               "unit %x = vcall interface @I %i:1()"
    val expectedCode = "interface @I %i = upcast null\n" +
                       "struct @I.vtable_type$* %x.vtable$#1 = scall @tungsten.load_ivtable(interface @I %i, \"I\")\n" +
                       "(interface @I)->unit %x.method$#2 = loadelement struct @I.vtable_type$* %x.vtable$#1, int64 0, int64 3\n" +
                       "unit %x = pcall (interface @I)->unit %x.method$#2(interface @I %i)"
    testConversion(expectedCode, code)
  }

  @Test
  def testPCallTypeParameters {
    val code = "class @C %x = upcast null\n" +
               "class @C %y = pcall [@g.T](type @g.T)->type @g.T @g[class @C](class @C %x)\n"
    val expectedCode = "class @C %x = upcast null\n" +
                       "type @g.T %y.cast$#1 = bitcast class @C %x\n" +
                       "type @g.T %y.cast$#2 = pcall [@g.T](type @g.T)->type @g.T @g(type @g.T %y.cast$#1)\n" +
                       "class @C %y = bitcast type @g.T %y.cast$#2"
    testConversion(expectedCode, code)
  }

  @Test
  def testSCallTypeParameters {
    val code = "class @C %x = upcast null\n" +
               "class @C %y = scall @g[class @C](class @C %x)\n"
    val expectedCode = "class @C %x = upcast null\n" +
                       "type @g.T %y.cast$#1 = bitcast class @C %x\n" +
                       "type @g.T %y.cast$#2 = scall @g(type @g.T %y.cast$#1)\n" +
                       "class @C %y = bitcast type @g.T %y.cast$#2\n"
    testConversion(expectedCode, code)
  }

  @Test
  def testVCallTypeParameters {
    val code = "class @C %x = upcast null\n" +
               "class @C %y = vcall class @C %x:2[class @C](class @C %x)\n"
    val expectedCode = "class @C %x = upcast null\n" +
                       "struct @C.vtable_type$* %y.vtable$#1 = loadelement class @C %x, int64 0, int64 0\n" +
                       "[@C.id.T](class @C, type @C.id.T)->type @C.id.T %y.method$#2 = loadelement struct @C.vtable_type$* %y.vtable$#1, int64 0, int64 4\n" +
                       "type @C.id.T %y.cast$#3 = bitcast class @C %x\n" +
                       "type @C.id.T %y.cast$#4 = pcall [@C.id.T](class @C, type @C.id.T)->type @C.id.T %y.method$#2(class @C %x, type @C.id.T %y.cast$#3)\n" +
                       "class @C %y = bitcast type @C.id.T %y.cast$#4\n"
    testConversion(expectedCode, code)
  }

  @Test
  def substituteClassType {
    val module = makeModule("return ()")
    val ty = ClassType("C")
    val expectedTy = PointerType(StructType("C.data$"))
    assertEquals(expectedTy, pass.substituteType(ty, module))
  }

  @Test
  def substituteInterfaceType {
    val module = makeModule("return ()")
    val ty = InterfaceType("I")
    val expectedTy = PointerType(StructType("R.data$"))
    assertEquals(expectedTy, pass.substituteType(ty, module))
  }

  @Test
  def substituteVariableType {
    val module = makeModule("return ()")
    val ty = VariableType("g.T")
    val expectedTy = PointerType(StructType("R.data$"))
    assertEquals(expectedTy, pass.substituteType(ty, module))
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
    assertEquals(expectedTy, pass.substituteType(ty, module))
  }
}
