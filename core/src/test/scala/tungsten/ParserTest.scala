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

import org.junit.Test
import org.junit.Assert._
import java.io.File
import Utilities._

class ParserTest {
  val parser = new Parser

  def test[T](input: String, parse: parser.Parser[T], expected: T) {
    val scanner = new Lexer.Scanner(input)
    parser.phrase(parse)(scanner) match {
      case parser.Success(r, _) => assertEquals(expected, r)
      case error: parser.NoSuccess => fail(error.msg)
    }
  }

  def testType(input: String, expected: Type) {
    test(input, parser.ty, expected)
  }

  def testValue(input: String, expected: Value) {
    test(input, parser.value, expected)
  }

  def testInstruction(input: String, expected: Instruction) {
    test(input, parser.instruction, expected)
  }

  def testDefinition(input: String, 
                     parse: parser.Parser[AstNode], 
                     expected: Definition) 
  {
    val scanner = new Lexer.Scanner(input)
    parser.phrase(parse)(scanner) match {
      case parser.Success(node, _) => assertEquals(expected, node.definition)
      case error: parser.NoSuccess => fail(error.msg)
    }
  }

  @Test
  def reserved {
    test("{", parser.reserved("{"), "{")
  }

  @Test
  def symbol {
    test("@x", parser.symbol, Symbol("@x"))
  }

  @Test
  def integer {
    test("123", parser.integer, 123L)
  }

  @Test
  def float {
    test("1.5", parser.float, 1.5)
  }

  @Test
  def char {
    test("'a'", parser.char, 'a')
  }

  @Test
  def string {
    test("\"hello\"", parser.string, "hello")
  }

  @Test
  def unitType {
    testType("unit", UnitType)
  }

  @Test
  def booleanType {
    testType("boolean", BooleanType)
  }

  @Test
  def charType {
    testType("char", CharType)
  }

  @Test
  def stringType {
    testType("string", StringType)
  }

  @Test
  def intTypes {
    testType("int8", IntType(8))
    testType("int16", IntType(16))
    testType("int32", IntType(32))
    testType("int64", IntType(64))
  }

  @Test
  def floatTypes {
    testType("float32", FloatType(32))
    testType("float64", FloatType(64))
  }

  @Test
  def nullType {
    testType("nulltype", NullType)
  }

  @Test
  def structType {
    testType("struct @T", StructType(Symbol("@T")))
  }

  @Test
  def functionType {
    testType("()->unit", FunctionType(UnitType, Nil, Nil))
    testType("[@S, @T](unit, unit)->unit", 
             FunctionType(UnitType, 
                          List("@S", "@T"),
                          List(UnitType, UnitType)))
    testType("()->()->unit", FunctionType(FunctionType(UnitType, Nil, Nil), Nil, Nil))
    testType("->->unit", FunctionType(FunctionType(UnitType, Nil, Nil), Nil, Nil))
  }

  @Test
  def arrayType {
    testType("[12 x unit]", ArrayType(12L, UnitType))
  }

  @Test
  def pointerType {
    testType("unit**", PointerType(PointerType(UnitType)))
  }

  @Test
  def nullableType {
    testType("unit*?", PointerType(UnitType, ReferenceType.NULLABLE))
  }

  @Test
  def classType {
    testType("class @T", ClassType("@T"))
    testType("class @T[nothing, nothing]",
             ClassType("@T", List(NothingType(), NothingType())))
  }

  @Test
  def nullableClassType {
    testType("class? @T", ClassType("@T", pointerFlags=ReferenceType.NULLABLE))
  }

  @Test
  def interfaceType {
    testType("interface @I", InterfaceType("@I"))
    testType("interface @I[nothing, nothing]",
             InterfaceType("@I", List(NothingType(), NothingType())))
  }

  @Test
  def variableType {
    testType("type @T", VariableType("@T"))
  }

  @Test
  def unitValue {
    testValue("()", UnitValue)
  }

  @Test
  def booleanValues {
    testValue("true", BooleanValue(true))
    testValue("false", BooleanValue(false))
  }

  @Test
  def charValue {
    testValue("'a'", CharValue('a'))
  }

  @Test
  def stringValue {
    testValue("\"hello\"", ArrayValue(IntType(8),
                                      List(IntValue(104, 8),
                                           IntValue(101, 8),
                                           IntValue(108, 8),
                                           IntValue(108, 8),
                                           IntValue(111, 8))))
    testValue("\"a\\0ab\"", ArrayValue(IntType(8),
                                       List(IntValue(97, 8),
                                            IntValue(10, 8),
                                             IntValue(98, 8))))
    testValue("\"θ\"", ArrayValue(IntType(8),
                                  List(IntValue(-50, 8),
                                       IntValue(-72, 8))))
  }

  @Test
  def integerValues {
    testValue("int8 12", IntValue(12L, 8))
    testValue("int16 12", IntValue(12L, 16))
    testValue("int32 12", IntValue(12L, 32))
    testValue("int64 12", IntValue(12L, 64))
  }

  @Test
  def floatValues {
    testValue("float32 1.5", FloatValue(1.5, 32))
    testValue("float64 1.5", FloatValue(1.5, 64))
  }

  @Test
  def nullValue {
    testValue("null", NullValue)
  }

  @Test
  def arrayValue {
    testValue("[0 x unit] {}", ArrayValue(UnitType, Nil))
    testValue("[1 x unit] {()}", ArrayValue(UnitType, List(UnitValue)))
    testValue("[2 x unit] {(), ()}", ArrayValue(UnitType, List(UnitValue, UnitValue)))
  }

  @Test
  def structValue {
    testValue("struct @T {(), ()}", StructValue("@T", List(UnitValue, UnitValue)))
  }

  @Test
  def definedValue {
    testValue("unit %x", DefinedValue("%x", UnitType))
  }

  @Test
  def bitCastValue {
    testValue("bitcast () to unit", BitCastValue(UnitValue, UnitType))
  }

  @Test
  def annotationValue {
    test("@x", parser.annotationValue, AnnotationValue("@x", Nil))
    test("@x(true)", parser.annotationValue, AnnotationValue("@x", List(BooleanValue(true))))
  }

  @Test
  def instName {
    val anns = List(AnnotationValue("@ann", List(UnitValue)))
    val ty = UnitType
    val n = Symbol("%x")
    test("@ann(()) unit %x = address", parser.instName("address"), 
         parser.~(parser.~(anns, ty), n))
  }

  @Test
  def instNameOpt {
    val expected = parser.~(parser.~(Nil, UnitType), Symbol("%anon$", 1))
    test("address", parser.instName("address"), expected)
  }

  @Test
  def addressInst {
    testInstruction("unit %x = address (), ()",
                    AddressInstruction("%x", UnitType, UnitValue, List(UnitValue)))
  }

  @Test
  def binopInst {
    testInstruction("unit %x = binop () % ()",
                    BinaryOperatorInstruction("%x", UnitType, BinaryOperator.REMAINDER,
                                              UnitValue, UnitValue))
  }

  @Test
  def bitcastInst {
    testInstruction("unit %x = bitcast ()",
                    BitCastInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def branchInst {
    testInstruction("unit %x = branch %a(())",
                    BranchInstruction("%x", UnitType, "%a", List(UnitValue)))
  }

  @Test
  def condInst {
    testInstruction("unit %x = cond true ? %a(()) : %b(())",
                    ConditionalBranchInstruction("%x",
                                                 UnitType,
                                                 BooleanValue(true),
                                                 "%a",
                                                 List(UnitValue),
                                                 "%b",
                                                 List(UnitValue)))
  }

  @Test
  def extractInst {
    testInstruction("unit %x = extract (), ()",
                    ExtractInstruction("%x", UnitType, UnitValue, List(UnitValue)))
  }

  @Test
  def fextendInst {
    testInstruction("unit %x = fextend ()",
                    FloatExtendInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def ftoiInst {
    testInstruction("unit %x = ftoi ()",
                    FloatToIntegerInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def ftruncateInst {
    testInstruction("unit %x = ftruncate ()",
                    FloatTruncateInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def heapInst {
    testInstruction("unit %x = heap",
                    HeapAllocateInstruction("%x", UnitType))
  }

  @Test
  def heapArrayInst {
    testInstruction("unit %x = heaparray ()",
                    HeapAllocateArrayInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def itofInst {
    testInstruction("unit %x = itof ()",
                    IntegerToFloatInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def isextendInst {
    testInstruction("unit %x = isextend ()",
                    IntegerSignExtendInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def itruncateInst {
    testInstruction("unit %x = itruncate ()",
                    IntegerTruncateInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def izextendInst {
    testInstruction("unit %x = izextend ()",
                    IntegerZeroExtendInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def insertTest {
    testInstruction("unit %x = insert (), (), ()",
                    InsertInstruction("%x", UnitType, UnitValue, UnitValue, List(UnitValue)))
  }

  @Test
  def instanceofTest {
    testInstruction("unit %x = instanceof (): unit",
                    InstanceOfInstruction("%x", UnitType, UnitValue, UnitType))
  }

  @Test
  def intrinsicInst {
    testInstruction("unit %x = intrinsic exit(())",
                    IntrinsicCallInstruction("%x", UnitType, Intrinsic.EXIT, List(UnitValue)))
  }

  @Test
  def loadInst {
    testInstruction("unit %x = load ()",
                    LoadInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def loadElementInst {
    testInstruction("unit %x = loadelement (), ()",
                    LoadElementInstruction("%x", UnitType, UnitValue, List(UnitValue)))
  }

  @Test
  def newInst {
    testInstruction("class @A %x = new @A.ctor[nothing](())",
                    NewInstruction("%x", ClassType("@A"), "@A.ctor", List(NothingType()), List(UnitValue)))
  }

  @Test
  def nullcheckInst {
    testInstruction("unit %x = nullcheck ()",
                    NullCheckInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def pcallInst {
    testInstruction("unit %x = pcall ()[nothing](())",
                    PointerCallInstruction("%x", UnitType, UnitValue, List(NothingType()), List(UnitValue)))
  }

  @Test
  def relopInst {
    testInstruction("unit %x = relop () < ()",
                    RelationalOperatorInstruction("%x",
                                                  UnitType,
                                                  RelationalOperator.LESS_THAN,
                                                  UnitValue, UnitValue))
  }

  @Test
  def returnInst {
    testInstruction("unit %x = return ()",
                    ReturnInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def storeInst {
    testInstruction("unit %x = store (), ()",
                    StoreInstruction("%x", UnitType, UnitValue, UnitValue))
  }

  @Test
  def storeElementInst {
    testInstruction("unit %x = storeelement (), (), ()",
                    StoreElementInstruction("%x", UnitType, UnitValue, UnitValue, List(UnitValue)))
  }

  @Test
  def stackInst {
    testInstruction("unit %x = stack",
                    StackAllocateInstruction("%x", UnitType))
  }

  @Test
  def stackArrayInst {
    testInstruction("unit %x = stackarray ()",
                    StackAllocateArrayInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def staticCallInst {
    testInstruction("unit %x = scall @f[nothing](())",
                    StaticCallInstruction("%x", UnitType, "@f", List(NothingType()), List(UnitValue)))
  }

  @Test
  def staticCallInstWithoutTypeArguments {
    testInstruction("unit %x = scall @f(())",
                    StaticCallInstruction("%x", UnitType, "@f", Nil, List(UnitValue)))
  }

  @Test
  def throwInst {
    testInstruction("unit %x = throw ()",
                    ThrowInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def upcastInst {
    testInstruction("unit %x = upcast ()",
                    UpcastInstruction("%x", UnitType, UnitValue))
  }

  @Test
  def vcallInst {
    testInstruction("unit %x = vcall ():0[nothing](())",
                    VirtualCallInstruction("%x", UnitType, UnitValue, 0, List(NothingType()), List(UnitValue)))
  }

  @Test
  def vlookupInst {
    testInstruction("unit %x = vlookup ():0",
                    VirtualLookupInstruction("%x", UnitType, UnitValue, 0))
  }

  @Test
  def parameter {
    testDefinition("@ann unit %p", parser.parameter,
                   Parameter("%p", UnitType, List(AnnotationValue("@ann", Nil))))
  }

  @Test
  def typeParameter {
    import Variance._
    testDefinition("type @T", parser.typeParameter,
                   TypeParameter("@T", None, None, INVARIANT))
    testDefinition("type +@T", parser.typeParameter,
                   TypeParameter("@T", None, None, COVARIANT))
    testDefinition("type -@T", parser.typeParameter,
                   TypeParameter("@T", None, None, CONTRAVARIANT))
    testDefinition("type @T <: type @U", parser.typeParameter,
                   TypeParameter("@T", Some(VariableType("@U")), None, INVARIANT))
    testDefinition("@ann type @T <: type @U", parser.typeParameter,
                   TypeParameter("@T", Some(VariableType("@U")), None, INVARIANT,
                                 List(AnnotationValue("@ann", Nil))))
    testDefinition("type @T >: type @L", parser.typeParameter,
                   TypeParameter("@T", None, Some(VariableType("@L")), INVARIANT))
    testDefinition("type @T <: type @U >: type @L", parser.typeParameter,
                   TypeParameter("@T", Some(VariableType("@U")), Some(VariableType("@L")), INVARIANT))
    testDefinition("@ann type @T <: type @U >: type @L", parser.typeParameter,
                   TypeParameter("@T", Some(VariableType("@U")), Some(VariableType("@L")), INVARIANT,
                                 List(AnnotationValue("@ann", Nil))))
  }

  @Test
  def block {
    testDefinition("block %b(unit %x) {\n" +
                   "  unit %i = return ()\n" +
                   "}", 
                   parser.block,
                   Block("%b", List("%x"), List("%i")))
  }

  @Test
  def blockWithCatch {
    testDefinition("block %b {\n" +
                   "  unit %i = return ()\n" +
                   "} catch @f.c((), ())",
                   parser.block,
                   Block("%b", Nil, List("%i"), Some("@f.c", List(UnitValue, UnitValue))))
  }

  @Test
  def field {
    testDefinition("field unit %a", parser.field,
                   Field("%a", UnitType))
  }

  @Test
  def annotation {
    testDefinition("annotation @ann(unit %a, unit %b)", parser.annotation,
                   Annotation("@ann", List("%a", "%b")))
  }

  @Test
  def function {
    testDefinition("function unit @f(unit %a, unit %b) {\n" +
                   "  block %c() {\n" +
                   "    return ()\n" +
                   "  }\n" +
                   "  block %d() {\n" +
                   "    return ()\n" +
                   "  }\n" +
                   "}\n",
                   parser.function,
                   Function("@f", UnitType, Nil, List("%a", "%b"), List("%c", "%d"), Nil))
  }

  @Test
  def functionWithTypeParameter {
    testDefinition("function type %T @id[type %T](type %T %x) {\n" +
                   "  block %entry {\n" +
                   "    return type %T %x\n" +
                   "  }\n" +
                   "}\n",
                   parser.function,
                   Function("@id",
                            VariableType("%T"),
                            List("%T"),
                            List("%x"),
                            List("%entry"),
                            Nil))
  }

  @Test
  def global {
    testDefinition("global unit @g", parser.global,
                   Global("@g", UnitType, None))
    testDefinition("global unit @g = ()", parser.global,
                   Global("@g", UnitType, Some(UnitValue)))
  }

  @Test
  def struct {
    testDefinition("struct @s {\n" +
                   "  field unit %a\n" +
                   "  field unit %b\n" +
                   "}\n",
                   parser.struct,
                   Struct("@s", List("%a", "%b")))
  }

  @Test
  def emptyInterface {
    val expected = Interface("@I", Nil, ClassType("@C"), Nil, Nil, Nil)
    val code = "interface @I <: class @C"
    testDefinition(code, parser.interface, expected)
  }

  @Test
  def interfaceWithSuperclass {
    val expected = Interface("@I", Nil, InterfaceType("@J"), Nil, Nil, Nil)
    val code = "interface @I <: interface @J"
    testDefinition(code, parser.interface, expected)
  }

  @Test
  def interface {
    val expected = Interface("@I",
                             List("@T"),
                             ClassType("@C", List(VariableType("@T"))),
                             List(InterfaceType("@J", List(VariableType("@T"))),
                                  InterfaceType("@K")),
                             List(List("@I.m1", "@I.m2"),
                                  List("@I.m3", "@I.m4")),
                             List("@I.m1", "@I.m2", "@I.m3", "@I.m4"),
                             List(AnnotationValue("@ann", Nil)))
    val code = "@ann interface @I[type @T] <: class @C[type @T] {\n" +
               "  interface @J[type @T] {\n" +
               "    @I.m1, @I.m2\n" +
               "  }\n" +
               "  interface @K {\n" +
               "    @I.m3, @I.m4\n" +
               "  }\n" +
               "  methods {\n" +
               "    @I.m1, @I.m2, @I.m3, @I.m4\n" +
               "  }\n" +
               "}\n"
    testDefinition(code, parser.interface, expected)
  }

  @Test
  def emptyClass {
    val expected = Class("@C", Nil, None, Nil, Nil, Nil, Nil, Nil)
    val code = "class @C"
    testDefinition(code, parser.clas, expected)
  }

  @Test
  def clas {
    val expected = Class("@C",
                         List("@T"),
                         Some(ClassType("@S", List(VariableType("@T")))),
                         List(InterfaceType("@I", List(ClassType("@C"))),
                              InterfaceType("@J")),
                         List(List("@C.m1", "@C.m2"),
                              List("@C.m3", "@C.m4")),
                         List("@C.c1", "@C.c2"),
                         List("@C.m1", "@C.m2", "@C.m3", "@C.m4"),
                         List("@C.f1", "@C.f2"),
                         List(AnnotationValue("@ann", Nil)))
    val code = "@ann class @C[type @T] <: class @S[type @T]{\n" +
               "  interface @I[class @C] {\n" +
               "    @C.m1, @C.m2\n" +
               "  }\n" +
               "  interface @J {\n" +
               "    @C.m3, @C.m4\n" +
               "  }\n" +
               "  constructors {\n" +
               "    @C.c1, @C.c2\n" +
               "  }\n" +
               "  methods {\n" +
               "    @C.m1, @C.m2, @C.m3, @C.m4\n" +
               "  }\n" +
               "  field unit @C.f1\n" +
               "  field unit @C.f2\n" +
               "}\n"
    testDefinition(code, parser.clas, expected)
  }

  @Test
  def emptyModule {
    test("", parser.module,
         (new Module(), Nil))
  }

  @Test
  def headers {
    test("name: @m\n" +
         "type: library\n" +
         "version: v0.1\n" +
         "filename: \"foo.w\"\n" +
         "dependencies: -lfoo, -lbar:0.1-, -lbaz:-1.0\n" +
         "searchpaths: \"/foo/bar\", \"/baz\"\n" +
         "is64bit: true\n" +
         "safe: true\n",
         parser.headers,
         new Module(name=Symbol("m"),
                    ty=ModuleType.LIBRARY,
                    version=Version(0, 1),
                    filename=Some(new File("foo.w")),
                    dependencies=List(ModuleDependency("foo", Version.MIN, Version.MAX),
                                      ModuleDependency("bar", Version(0, 1), Version.MAX),
                                      ModuleDependency("baz", Version.MIN, Version(1, 0))),
                    searchPaths=List(new File("/foo/bar"), new File("/baz")),
                    is64Bit=true,
                    isSafe=true))
  }
}

class AstNodeTest {
  val childDefn = Parameter("%x", UnitType)
  val child = AstNode(childDefn, Nil)
  val parentDefn = Function("@f", UnitType, Nil, List("%x"), Nil)
  val parent = AstNode(parentDefn, List(child))

  @Test
  def globalizeSymbol {
    assertEquals(symbolFromString("bar"), parent.globalizeSymbol("@bar", None))
    assertEquals(symbolFromString("bar"), parent.globalizeSymbol("%bar", None))
    assertEquals(symbolFromString("bar"), parent.globalizeSymbol("@bar", Some("foo")))
    assertEquals(symbolFromString("foo.bar"), parent.globalizeSymbol("%bar", Some("foo")))
  }

  @Test
  def globalize {
    val expectedChildDefn = Parameter("f.x", UnitType)
    val expectedChild = AstNode(expectedChildDefn, Nil)
    val expectedParentDefn = Function("f", UnitType, Nil, List("f.x"), Nil)
    val expectedParent = AstNode(expectedParentDefn, List(expectedChild))
    assertEquals(expectedParent, parent.globalize(None))
  }
}
