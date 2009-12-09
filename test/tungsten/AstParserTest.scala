package tungsten

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

class AstParserTest {
  val fooLoc = Location("foo.w", 1, 2, 3, 4)
  val foo = new Symbol("foo")

  def test[T](input: String, parser: AstParser.Parser[T], expected: T) = {
    val scanner = new AstLexer.Scanner(input)
    val result = AstParser.phrase(parser)(scanner)
    result match {
      case AstParser.Success(ast, _) => assertEquals(expected, ast)
      case AstParser.Failure(message, _) => fail(message)
      case AstParser.Error(message, _) => fail(message)
    }
  } 

  def testModule(input: String, expected: AstModule) = {
    test(input, AstParser.module, expected)
  }

  def testType(input: String, expected: AstType) = {
    test(input, AstParser.ty, expected)
  }

  def testValue(input: String, expected: AstValue) = {
    test(input, AstParser.value, expected)
  }

  def testInstruction(input: String, expected: AstInstruction) = {
    test(input, AstParser.instruction, expected)
  }

  @Test
  def empty = {
    testModule("", AstModule(Nil))
  }

  @Test
  def whitespace = {
    testModule(" \t\n", AstModule(Nil))
  }

  @Test
  def types = {
    testType("#unit", AstUnitType(Nowhere))
    testType("#boolean", AstBooleanType(Nowhere))
    testType("#int32", AstIntType(32, Nowhere))
    testType("#int64", AstIntType(64, Nowhere))
    testType("#float32", AstFloatType(32, Nowhere))
    testType("#float64", AstFloatType(64, Nowhere))
    testType("#null", AstNullType(Nowhere))
    testType("[12 * #int32]", AstArrayType(Some(12), AstIntType(32, Nowhere), Nowhere))
    testType("[? * #int32]", AstArrayType(None, AstIntType(32, Nowhere), Nowhere))
    testType("Foo", AstClassType(new Symbol("Foo"), Nil, Nowhere))
    testType("foo.bar.Baz", AstClassType(Symbol(List("foo", "bar", "Baz"), 0), Nil, Nowhere))
    testType("Foo[Baz]", AstClassType(new Symbol("Foo"), 
                                      List(AstClassType(new Symbol("Baz"), Nil, Nowhere)),
                                      Nowhere))
  }

  @Test
  def typeWithLocation = {
    testType("#unit <foo.w:1.2-3.4>", AstUnitType(fooLoc))
    testType("Foo <foo.w:1.2-3.4>", AstClassType(new Symbol("Foo"), Nil, fooLoc))
  }

  @Test
  def pointerType = {
    testType("#unit**", AstPointerType(AstPointerType(AstUnitType(Nowhere), Nowhere), Nowhere))
  }

  @Test
  def pointerTypeWithLocations = {
    val l1 = Location("foo.w", 1, 2, 3, 4)
    val l2 = Location("foo.w", 1, 2, 3, 5)
    val l3 = Location("foo.w", 1, 2, 3, 6)
    testType("#unit <foo.w:1.2-3.4> * <foo.w:1.2-3.5> * <foo.w:1.2-3.6>",
             AstPointerType(AstPointerType(AstUnitType(l1), l2), l3))
  }

  @Test
  def value = {
    testValue("()", AstUnitValue(Nowhere))
    testValue("#true", AstBooleanValue(true, Nowhere))
    testValue("12b", AstInt8Value(12, Nowhere))
    testValue("12s", AstInt16Value(12, Nowhere))
    testValue("12", AstInt32Value(12, Nowhere))
    testValue("12L", AstInt64Value(12L, Nowhere))
    testValue("1.5f", AstFloat32Value(1.5f, Nowhere))
    testValue("1.5", AstFloat64Value(1.5, Nowhere))
    testValue("#null", AstNullValue(Nowhere))
    testValue("[#unit:]", AstArrayValue(AstUnitType(Nowhere), Nil, Nowhere))
    testValue("[#unit: ()]", 
              AstArrayValue(AstUnitType(Nowhere), List(AstUnitValue(Nowhere)), Nowhere))
    testValue("foo", AstSymbolValue(new Symbol("foo"), Nowhere))
  }

  @Test
  def valueWithLocation = {
    val expected = AstUnitValue(fooLoc)
    testValue("() <foo.w:1.2-3.4>", expected)
  }

  @Test
  def addressInst = {
    testInstruction("#address <foo.w:1.2-3.4> foo = (), (), ()",
                    AstAddressInstruction(foo, 
                                          AstUnitValue(Nowhere),
                                          List(AstUnitValue(Nowhere), AstUnitValue(Nowhere)),
                                          fooLoc))
  }

  @Test
  def assignInst = {
    testInstruction("#assign <foo.w:1.2-3.4> foo = 123",
                    AstAssignInstruction(foo, AstInt32Value(123, Nowhere), fooLoc))
  }

  @Test
  def binopInst = {
    testInstruction("#binop <foo.w:1.2-3.4> foo = 12 + 34",
                    AstBinaryOperatorInstruction(foo,
                                                 BinaryOperator.ADD,
                                                 AstInt32Value(12, Nowhere),
                                                 AstInt32Value(34, Nowhere),
                                                 fooLoc))
  }

  @Test
  def binopRemainder = {
    testInstruction("#binop foo = 12 % 34",
                    AstBinaryOperatorInstruction(foo,
                                                 BinaryOperator.REMAINDER,
                                                 AstInt32Value(12, Nowhere),
                                                 AstInt32Value(34, Nowhere),
                                                 Nowhere))
  }

  @Test
  def branchInst = {
    testInstruction("#branch <foo.w:1.2-3.4> foo = bar(123)",
                    AstBranchInstruction(foo, 
                                         new Symbol("bar"),
                                         List(AstInt32Value(123, Nowhere)),
                                         fooLoc))
  }

  @Test
  def condBranchInst = {
    testInstruction("#cond <foo.w:1.2-3.4> foo = () ? bar(12) : baz(34)",
                    AstConditionalBranchInstruction(foo,
                                                    AstUnitValue(Nowhere),
                                                    new Symbol("bar"),
                                                    List(AstInt32Value(12, Nowhere)),
                                                    new Symbol("baz"),
                                                    List(AstInt32Value(34, Nowhere)),
                                                    fooLoc))
  }

  @Test
  def indirectCallInst = {
    testInstruction("#icall <foo.w:1.2-3.4> foo = bar(123)",
                    AstIndirectCallInstruction(foo,
                                               AstSymbolValue(new Symbol("bar"), Nowhere),
                                               List(AstInt32Value(123, Nowhere)),
                                               fooLoc))
  }

  @Test
  def intrinsicInst = {
    testInstruction("#intrinsic <foo.w:1.2-3.4> foo = exit(1)",
                    AstIntrinsicCallInstruction(foo,
                                                new Symbol("exit"),
                                                List(AstInt32Value(1, Nowhere)),
                                                fooLoc))
  }

  @Test
  def loadInst = {
    testInstruction("#load <foo.w:1.2-3.4> foo = *()",
                    AstLoadInstruction(foo, AstUnitValue(Nowhere), fooLoc))
  }

  @Test
  def loadElementInst = {
    testInstruction("#loadelement <foo.w:1.2-3.4> foo = (), (), ()",
                    AstLoadElementInstruction(foo,
                                              AstUnitValue(Nowhere),
                                              List(AstUnitValue(Nowhere), AstUnitValue(Nowhere)),
                                              fooLoc))
  }

  @Test
  def relopInst = {
    testInstruction("#relop <foo.w:1.2-3.4> foo = 12 == 34",
                    AstRelationalOperatorInstruction(foo,
                                                     RelationalOperator("=="),
                                                     AstInt32Value(12, Nowhere),
                                                     AstInt32Value(34, Nowhere),
                                                     fooLoc))
  }

  @Test
  def returnInst = {
    testInstruction("#return <foo.w:1.2-3.4> foo = 123", 
                    AstReturnInstruction(foo, AstInt32Value(123, Nowhere), fooLoc))
  }

  @Test
  def stackAllocateInst = {
    testInstruction("#stack <foo.w:1.2-3.4> foo : #int32*",
                    AstStackAllocateInstruction(foo, 
                                                AstPointerType(AstIntType(32, Nowhere),
                                                               Nowhere),
                                                fooLoc))
  }

  @Test
  def stackArrayAllocateInst = {
    testInstruction("#stackarray <foo.w:1.2-3.4> foo = () * #unit",
                    AstStackAllocateArrayInstruction(foo,
                                                     AstUnitValue(Nowhere),
                                                     AstUnitType(Nowhere),
                                                     fooLoc))
  }

  @Test
  def staticCallInst = {
    testInstruction("#scall <foo.w:1.2-3.4> foo = bar(123)",
                    AstStaticCallInstruction(foo,
                                             new Symbol("bar"),
                                             List(AstInt32Value(123, Nowhere)),
                                             fooLoc))
  }

  @Test
  def storeInst = {
    testInstruction("#store <foo.w:1.2-3.4> foo = *() <- ()",
                    AstStoreInstruction(foo,
                                        AstUnitValue(Nowhere),
                                        AstUnitValue(Nowhere),
                                        fooLoc))
  }

  @Test
  def storeElementInst = {
    testInstruction("#storeelement <foo.w:1.2-3.4> foo = (), (), () <- ()",
                    AstStoreElementInstruction(foo,
                                               AstUnitValue(Nowhere),
                                               List(AstUnitValue(Nowhere), AstUnitValue(Nowhere)),
                                               AstUnitValue(Nowhere),
                                               fooLoc))
  }

  @Test
  def upcastInst = {
    testInstruction("#upcast <foo.w:1.2-3.4> foo = #null : #null",
                    AstUpcastInstruction(foo,
                                         AstNullValue(Nowhere),
                                         AstNullType(Nowhere),
                                         fooLoc))
  }

  @Test
  def parameter = {
    val tyLoc = Location("foo.w", 5, 6, 7, 8)
    val expected = AstParameter(new Symbol("foo"), AstIntType(32, tyLoc), fooLoc)
    test("foo <foo.w:1.2-3.4> : #int32 <foo.w:5.6-7.8>", AstParser.parameter, expected)
  }

  @Test
  def block = {
    val program = "#block <foo.w:1.2-3.4> foo(bar : #int32,\n" +
                  "                           baz : #int32) {\n" +
                  "  #return quux = 123\n" +
                  "}\n"
    val (p1, p2) = (AstParameter(new Symbol("bar"), new AstIntType(32, Nowhere), Nowhere),
                    AstParameter(new Symbol("baz"), new AstIntType(32, Nowhere), Nowhere))
    val expected = AstBlock(new Symbol("foo"),
                            List(p1, p2),
                            List(AstReturnInstruction(new Symbol("quux"),
                                                      AstInt32Value(123, Nowhere),
                                                      Nowhere)),
                            fooLoc)
    test(program, AstParser.block, expected)
  }

  @Test
  def typeParameter = {
    val param = "foo <foo.w:1.2-3.4> <: #unit >: #unit"
    val expected = AstTypeParameter(new Symbol("foo"),
                                    Some(AstUnitType(Nowhere)),
                                    Some(AstUnitType(Nowhere)),
                                    fooLoc)
    test(param, AstParser.typeParameter, expected)
  }

  @Test
  def global = {
    val global = AstGlobal(new Symbol("foo"), AstUnitType(Nowhere), None, Nowhere)
    val expected = AstModule(List(global))
    testModule("#global foo: #unit", expected)
  }

  @Test
  def globalWithLocation = {
    val global = AstGlobal(new Symbol("foo"), AstUnitType(Nowhere), None, fooLoc)
    val expected = AstModule(List(global))
    testModule("#global <foo.w:1.2-3.4> foo: #unit", expected)
  }

  @Test
  def globalWithValue = {
    val global = AstGlobal(new Symbol("foo"), 
                           AstUnitType(Nowhere),
                           Some(AstUnitValue(Nowhere)),
                           Nowhere)
    val expected = AstModule(List(global))
    testModule("#global foo: #unit = ()", expected)
  }

  @Test
  def functionEmpty = {
    val program = "#function foo( ): #unit { #block ret( ) { #return bar = () } }"
    val ret = AstBlock(new Symbol("ret"),
                       Nil,
                       List(AstReturnInstruction(new Symbol("bar"),
                                                 AstUnitValue(Nowhere),
                                                 Nowhere)),
                       Nowhere)
    val function = AstFunction(new Symbol("foo"), 
                               AstUnitType(Nowhere),
                               Nil,
                               Nil,
                               List(ret),
                               Nowhere)
    val expected = AstModule(List(function))
    testModule(program, expected)
  }

  @Test
  def function = {
    val program = "#function <foo.w:1.2-3.4> foo[T1, T2](bar: #unit, baz: #unit): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #branch quux = ret( )\n" + 
                  "  }\n" +
                  "  #block ret( ) {\n" +
                  "    #return xyz = ()\n" +
                  "  }\n" +
                  "}"
    val (t1, t2) = (AstTypeParameter(new Symbol("T1"), None, None, Nowhere),
                    AstTypeParameter(new Symbol("T2"), None, None, Nowhere))
    val (p1, p2) = (AstParameter(new Symbol("bar"), AstUnitType(Nowhere), Nowhere),
                    AstParameter(new Symbol("baz"), AstUnitType(Nowhere), Nowhere))
    val entry = AstBlock(new Symbol("entry"), Nil,
                         List(AstBranchInstruction(new Symbol("quux"),
                                                   new Symbol("ret"),
                                                   Nil, 
                                                   Nowhere)),
                         Nowhere)
    val ret = AstBlock(new Symbol("ret"), Nil,
                       List(AstReturnInstruction(new Symbol("xyz"),
                                                 AstUnitValue(Nowhere),
                                                 Nowhere)),
                       Nowhere)
    val function = AstFunction(new Symbol("foo"),
                               AstUnitType(Nowhere),
                               List(t1, t2),
                               List(p1, p2),
                               List(entry, ret),
                               fooLoc)
    val expected = AstModule(List(function))
    testModule(program, expected)
  }

  @Test
  def field = {
    val program = "#field <foo.w:1.2-3.4> foo: #unit"
    val expected = AstField(new Symbol("foo"), AstUnitType(Nowhere), fooLoc)
    test(program, AstParser.field, expected)
  }

  @Test
  def struct = {
    val program = "#struct <foo.w:1.2-3.4> Foo[T] {\n" +
                  "  #field bar: #unit,\n" +
                  "  #field baz: #unit\n" +
                  "}"
    val struct = AstStruct(new Symbol("Foo"),
                           List(AstTypeParameter(new Symbol("T"), None, None, Nowhere)),
                           List(AstField(new Symbol("bar"), AstUnitType(Nowhere), Nowhere),
                                AstField(new Symbol("baz"), AstUnitType(Nowhere), Nowhere)),
                           fooLoc)
    val expected = AstModule(List(struct))
    testModule(program, expected)
  }

  @Test
  def classBasic = {
    val program = "#class Foo {\n" +
                  "  #fields {\n" +
                  "  }\n" +
                  "  #methods {\n" +
                  "  }\n" +
                  "}"
    val clas = AstClass(new Symbol("Foo"),
                        Nil,
                        None,
                        Nil,
                        Nil,
                        Nil,
                        Nowhere)
    val expected = AstModule(List(clas))
    testModule(program, expected)
  }

  @Test
  def classFull = {
    val program = "#class <foo.w:1.2-3.4> Foo[T] <: Bar[T] : Baz, Quux[T] {\n" +
                  "  #fields {\n" +
                  "    #field a: #unit,\n" +
                  "    #field b: #unit\n" +
                  "  }\n" +
                  "  #methods {\n" +
                  "    #function c( ): #unit,\n" +
                  "    #function d( ): #unit\n" +
                  "  }\n" +
                  "}"
    val clas = AstClass(new Symbol("Foo"),
                        List(AstTypeParameter(new Symbol("T"), None, None, Nowhere)),
                        Some(AstClassType(new Symbol("Bar"),
                                          List(AstClassType(new Symbol("T"), Nil, Nowhere)),
                                          Nowhere)),
                        List(AstClassType(new Symbol("Baz"), Nil, Nowhere),
                             AstClassType(new Symbol("Quux"),
                                          List(AstClassType(new Symbol("T"), Nil, Nowhere)),
                                          Nowhere)),
                        List(AstField(new Symbol("a"),
                                      AstUnitType(Nowhere),
                                      Nowhere),
                             AstField(new Symbol("b"),
                                      AstUnitType(Nowhere),
                                      Nowhere)),
                        List(AstFunction(new Symbol("c"),
                                         AstUnitType(Nowhere),
                                         Nil,
                                         Nil,
                                         Nil,
                                         Nowhere),
                             AstFunction(new Symbol("d"),
                                         AstUnitType(Nowhere),
                                         Nil,
                                         Nil,
                                         Nil,
                                         Nowhere)),
                        fooLoc)
    val expected = AstModule(List(clas))
    testModule(program, expected)
  }

  @Test
  def interfaceBasic = {
    val program = "#interface Foo {}\n"
    val iface = AstInterface(new Symbol("Foo"), Nil, None, Nil, Nil, Nowhere)
    val expected = AstModule(List(iface))
    testModule(program, expected)
  }

  @Test
  def interfaceFull = {
    val program = "#interface <foo.w:1.2-3.4> Foo[T] <: Bar[T] : Baz, Quux[T] {\n" +
                  "  #function a( ): #unit,\n" +
                  "  #function b( ): #unit\n" +
                  "}"
    val iface = AstInterface(new Symbol("Foo"),
                             List(AstTypeParameter(new Symbol("T"), None, None, Nowhere)),
                             Some(AstClassType(new Symbol("Bar"),
                                               List(AstClassType(new Symbol("T"), Nil, Nowhere)),
                                               Nowhere)),
                             List(AstClassType(new Symbol("Baz"), Nil, Nowhere),
                                  AstClassType(new Symbol("Quux"),
                                               List(AstClassType(new Symbol("T"), Nil, Nowhere)),
                                               Nowhere)),
                             List(AstFunction(new Symbol("a"), 
                                              AstUnitType(Nowhere),
                                              Nil,
                                              Nil,
                                              Nil,
                                              Nowhere),
                                  AstFunction(new Symbol("b"),
                                              AstUnitType(Nowhere),
                                              Nil,
                                              Nil,
                                              Nil,
                                              Nowhere)),
                             fooLoc)
    val expected = AstModule(List(iface))
    testModule(program, expected)
  }
}
