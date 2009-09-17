package tungsten

import org.junit.Test
import org.junit.Assert._

class AstParserTest {
  val fooLoc = Location("foo.w", 1, 2, 3, 4)

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
    testType("#int32", AstIntType(32, Nowhere))
  }

  @Test
  def typeWithLocation = {
    testType("#unit <foo.w:1.2-3.4>", AstUnitType(fooLoc))
  }

  @Test
  def value = {
    testValue("()", AstUnitValue(Nowhere))
    testValue("123", AstIntValue(123, Nowhere))
    testValue("foo", AstSymbolValue(new Symbol("foo"), Nowhere))
  }

  @Test
  def valueWithLocation = {
    val expected = AstUnitValue(fooLoc)
    testValue("() <foo.w:1.2-3.4>", expected)
  }

  @Test
  def instruction = {
    testInstruction("#return <foo.w:1.2-3.4> 123", 
                    AstReturnInstruction(AstIntValue(123, Nowhere), fooLoc))
    testInstruction("#branch <foo.w:1.2-3.4> foo(123)",
                    AstBranchInstruction(AstSymbolValue(new Symbol("foo"), Nowhere),
                                         List(AstIntValue(123, Nowhere)), fooLoc))
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
                  "  #return 123\n" +
                  "}\n"
    val (p1, p2) = (AstParameter(new Symbol("bar"), new AstIntType(32, Nowhere), Nowhere),
                    AstParameter(new Symbol("baz"), new AstIntType(32, Nowhere), Nowhere))
    val expected = AstBlock(new Symbol("foo"),
                            List(p1, p2),
                            List(AstReturnInstruction(AstIntValue(123, Nowhere), Nowhere)),
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
    val program = "#function foo( ): #unit { #block ret( ) { #return () } }"
    val ret = AstBlock(new Symbol("ret"),
                       Nil,
                       List(AstReturnInstruction(AstUnitValue(Nowhere), Nowhere)),
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
                  "    #branch ret( )\n" + 
                  "  },\n" +
                  "  #block ret( ) {\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}"
    val (t1, t2) = (AstTypeParameter(new Symbol("T1"), None, None, Nowhere),
                    AstTypeParameter(new Symbol("T2"), None, None, Nowhere))
    val (p1, p2) = (AstParameter(new Symbol("bar"), AstUnitType(Nowhere), Nowhere),
                    AstParameter(new Symbol("baz"), AstUnitType(Nowhere), Nowhere))
    val entry = AstBlock(new Symbol("entry"), Nil,
                         List(AstBranchInstruction(AstSymbolValue(new Symbol("ret"), Nowhere),
                                                   Nil, Nowhere)), Nowhere)
    val ret = AstBlock(new Symbol("ret"), Nil,
                       List(AstReturnInstruction(AstUnitValue(Nowhere), Nowhere)), Nowhere)
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
}
