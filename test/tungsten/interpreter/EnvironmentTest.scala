package tungsten.interpreter

import org.junit.Test
import org.junit.Assert._
import tungsten._
import tungsten.Utilities._

class EnvironmentTest {
  val prefix = symbolFromString("main.entry")
      
  def prepare(program: String) = {
    val module = compileString(program)
    val env = new Environment(module)
    val mainFunction = module.getFunction("main")
    val entryBlock = module.getBlock(mainFunction.blocks.head)
    val firstInst = module.getInstruction(entryBlock.instructions.head)
    while (env.state.inst != firstInst)
      env.step
    (module, env)
  }

  def prepareCode(code: String) = {
    val program = "#function main( ): #unit { #block entry( ) { " + code + " } }"
    prepare(program)
  }

  def prepareDefault = {
    val code = "#return ()"
    prepareCode(code)
  }

  def testSimpleEval(code: String, expected: IValue) {
    val (module, env) = prepareCode(code)
    val instName = module.getBlock("main.entry").instructions.head
    env.step
    val value = env.state.get(instName)
    assertEquals(expected, value)
  }

  @Test
  def basicValues = {
    val (module, env) = prepareDefault
    assertEquals(tungsten.interpreter.UnitValue, env.create(tungsten.UnitValue()))
    assertEquals(tungsten.interpreter.BooleanValue(true), env.create(tungsten.BooleanValue(true)))
    assertEquals(tungsten.interpreter.Int8Value(12), env.create(tungsten.Int8Value(12)))
    assertEquals(tungsten.interpreter.Int16Value(12), env.create(tungsten.Int16Value(12)))
    assertEquals(tungsten.interpreter.Int32Value(12), env.create(tungsten.Int32Value(12)))
    assertEquals(tungsten.interpreter.Int64Value(12), env.create(tungsten.Int64Value(12)))
    assertEquals(tungsten.interpreter.Float32Value(1.0f), env.create(tungsten.Float32Value(1.0f)))
    assertEquals(tungsten.interpreter.Float64Value(1.0), env.create(tungsten.Float64Value(1.0)))
    assertEquals(tungsten.interpreter.NullReference, env.create(tungsten.NullValue()))
  }

  @Test
  def arrayValue = {
    val (module, env) = prepareDefault
    val value = tungsten.ArrayValue(IntType(32), List(tungsten.Int32Value(12)))
    val ivalue = env.create(value)
    val expected = tungsten.interpreter.ArrayValue(Array[IValue](tungsten.interpreter.Int32Value(12)))
    assertEquals(expected, ivalue)
  }

  @Test
  def structValue = {
    val program = "#struct A {\n" +
                  "  #field b: #unit\n" +
                  "}\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    val value = tungsten.StructValue("A", List(tungsten.UnitValue()))
    val ivalue = env.create(value)
    val expected = tungsten.interpreter.StructValue(Array[IValue](tungsten.interpreter.UnitValue))
    assertEquals(expected, ivalue)
  }

  @Test
  def setArguments = {
    val env = new Environment(new Module)
    val names = List("foo", "bar", "baz").map(Symbol(_))
    val args = List(1, 2, 3).map(Int32Value(_))
    env.setArguments(names, args)
    for ((n, a) <- names zip args)
      assertEquals(a, env.state.get(n))
  }

  @Test
  def branch = {
    val program = "#function main( ): #unit {\n" +
                  "  #block b1( ) { #return r1 = () }\n" +
                  "  #block b2(x: #int32, y: #int32) { #return r2 = () }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    val b1 = module.getBlock("main.b1")
    val b2 = module.getBlock("main.b2")
    env.branch(b2, List(Int32Value(12), Int32Value(34)))
    assertEquals(module.getInstruction(b2.instructions.head), env.state.inst)
    assertEquals(Int32Value(12), env.state.get("main.b2.x"))
    assertEquals(Int32Value(34), env.state.get("main.b2.y"))
  }

  @Test
  def branchKeepsParameters = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #scall f(12)\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n" +
                  "#function f(x: #int32): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #branch g( )\n" +
                  "  }\n" +
                  "  #block g( ) {\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step    // call
    env.step    // branch
    val x = env.state.get("f.x")
    assertEquals(Int32Value(12), x)
  }

  @Test
  def call = {
    val program = "#function main( ): #unit { #block entry( ) { #return r = () } }\n" +
                  "#function f(x: #int32): #unit { #block entry( ) { #return r = () } }\n"
    val (module, env) = prepare(program)
    val calledFunction = module.getFunction("f")
    val calledInst = module.getInstruction("f.entry.r")
    val oldSize = env.stack.size
    env.call(calledFunction, List(Int32Value(123)))
    assertEquals(oldSize + 1, env.stack.size)
    assertEquals(calledInst, env.state.inst)
    assertEquals(Int32Value(123), env.state.get("f.x"))
  }

  @Test
  def addressArrayInst = {
    val code = "#stackarray a = 2L * [2 * #int32]\n" +
               "#address b = a, 1L, 1L"
    val (module, env) = prepareCode(code)
    env.step
    env.step

    val a = env.state.get("main.entry.a").asInstanceOf[ScalarReference]
    val aArray1 = a.value.asInstanceOf[ArrayValue]
    val aArray2 = aArray1.value(1).asInstanceOf[ArrayValue]

    val b = env.state.get("main.entry.b").asInstanceOf[ArrayIndexReference]
    assertSame(aArray2, b.array)
    assertEquals(Int64Value(1), b.index)
  }

  @Test
  def addressStructInst = {
    val program = "#struct A {\n" +
                  "  #field x: #int32\n" +
                  "}\n" +
                  "#struct B {\n" +
                  "  #field y: A\n" +
                  "}\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #stack a: B*\n" +
                  "    #address b = a, 0L, 0L\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step
    env.step

    val a = env.state.get(prefix + "a").asInstanceOf[ScalarReference]
    val recordB = a.value.asInstanceOf[StructValue]
    val recordA = recordB.value(0).asInstanceOf[StructValue]

    val b = env.state.get(prefix + "b").asInstanceOf[StructIndexReference]
    val expected = StructIndexReference(recordA, Int64Value(0L))
    assertEquals(expected, b)
  }

  @Test
  def assignInst = {
    val (module, env) = prepareCode("#assign a = 123\n#assign b = a")
    env.step
    assertEquals(Int32Value(123), env.state.get(prefix + "a"))
    env.step
    assertEquals(Int32Value(123), env.state.get(prefix + "b"))
  }

  @Test
  def binopInst = {
    val (module, env) = prepareCode("#binop a = 1 + 2")
    env.step
    assertEquals(Int32Value(3), env.state.get(prefix + "a"))
  }

  @Test
  def floatBinopInst = {
    val (module, env) = prepareCode("#binop a = 1. + 2.")
    env.step
    assertEquals(Float64Value(3.), env.state.get(prefix + "a"))
  }

  @Test
  def branchInst = {
    val program = "#function main( ): #unit {\n" +
                  "  #block b1( ) { #branch b = b2(12, 34) }\n" +
                  "  #block b2(x: #int32, y: #int32) { #return r2 = () }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step
    val r2 = module.getInstruction("main.b2.r2")
    assertEquals(r2, env.state.inst)
    assertEquals(Int32Value(12), env.state.get("main.b2.x"))
  }

  @Test
  def condInst = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #cond c = #false ? foo(12) : bar(12)\n" +
                  "  }\n" +
                  "  #block foo(x: #int32) { #return r = () }\n" +
                  "  #block bar(x: #int32) { #return r = () }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step
    val barInst = module.getInstruction("main.bar.r")
    assertEquals(barInst, env.state.inst)
    assertEquals(Int32Value(12), env.state.get("main.bar.x"))
  }

  @Test
  def exitInst = {
    val (module, env) = prepareCode("#intrinsic x = exit(12)")
    env.step
    assertEquals(12, env.returnCode)
  }

  @Test
  def globalLoadInst = {
    val program = "#global foo: #int32 = 12\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #load a = *foo\n" +
                  "    #return r = ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    val foo = env.globalState("foo")
    assertEquals(Int32Value(12), foo.asInstanceOf[ScalarReference].value)
    env.step
    assertEquals(Int32Value(12), env.state.get("main.entry.a"))
  }

  @Test
  def globalStoreInst = {
    val program = "#global foo: #int32\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #store *foo <- 12\n" +
                  "    #return r = ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step
    val foo = env.globalState("foo")
    assertEquals(Int32Value(12), foo.asInstanceOf[ScalarReference].value)
  }

  @Test
  def floatExtendInst {
    testSimpleEval("#fextend a = 3.0f : #float64", Float64Value(3.0))
  }

  @Test
  def floatToIntInst {
    testSimpleEval("#ftoi a = 1.9f : #int32", Int32Value(1))
  }

  @Test
  def floatTruncateInst {
    testSimpleEval("#ftruncate a = 3.0 : #float32", Float32Value(3.0f))
  }

  @Test
  def heapAllocateInst {
    val (module, env) = prepareCode("#heap a : #int32*")
    env.step
    val a = env.state.get(prefix + "a").asInstanceOf[ScalarReference]
    assertEquals(Int32Value(0), a.value)
  }

  @Test
  def heapAllocateArrayInst {
    val (module, env) = prepareCode("#heaparray a = 12L * #unit")
    env.step
    val value = env.state.get(prefix + "a")
    assertTrue(value.isInstanceOf[ScalarReference])
    val arrayValue = value.asInstanceOf[ScalarReference].value
    assertTrue(arrayValue.isInstanceOf[ArrayValue])
    val array = arrayValue.asInstanceOf[ArrayValue].value
    assertEquals(12, array.length)
  }

  @Test
  def indirectCallInst = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #assign g = f\n" +
                  "    #icall res = g(12)\n" +
                  "    #return r = res\n" +
                  "  }\n" +
                  "}\n" +
                  "#function f(x: #int32): #int32 {\n" +
                  "  #block entry( ) {\n" +
                  "    #return r = x\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step    // assign
    val f = module.getFunction("f")
    assertEquals(FunctionValue(f), env.state.get("main.entry.g"))
    env.step    // call
    val r = module.getInstruction("f.entry.r")
    assertEquals(r, env.state.inst)
    assertEquals(Int32Value(12), env.state.get("f.x"))
  }

  @Test
  def intSignExtendInst {
    testSimpleEval("#isextend a = -12b : #int64", Int64Value(-12L))
  }

  @Test
  def intToFloatInst {
    testSimpleEval("#itof a = 3 : #float32", Float32Value(3.0f))
  }

  @Test
  def intTruncateInst {
    testSimpleEval("#itruncate a = 12L : #int8", Int8Value(12))
  }

  @Test
  def intZeroExtendInst {
    testSimpleEval("#izextend a = -1b : #int64", Int64Value(255))
  }

  @Test
  def loadInst = {
    val code = "#stack a : #int32*\n" +
               "#load b = *a"
    val (module, env) = prepareCode(code)
    env.step
    env.step
    assertEquals(Int32Value(0), env.state.get(prefix + "b"))
  }

  @Test
  def loadArrayInst = {
    val code = "#stack a: [1 * #int32]*\n" +
               "#address b = a, 0L\n" +
               "#load c = *b"
    val (module, env) = prepareCode(code)
    for (i <- 0 until 3) env.step
    assertEquals(Int32Value(0), env.state.get(prefix + "c"))
  }    

  @Test
  def loadElementArrayInst = {
    val code = "#loadelement a = [#int32: 12, 34], 0L"
    val (module, env) = prepareCode(code)
    env.step
    assertEquals(Int32Value(12), env.state.get(prefix + "a"))
  }

  @Test
  def loadElementStructInst = {
    val program = "#struct A {\n" +
                  "  #field b: #int32\n" +
                  "}\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #loadelement e = {A: 12}, 0L\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step
    assertEquals(Int32Value(12), env.state.get(prefix + "e"))
  }

  @Test
  def relopInst = {
    val (module, env) = prepareCode("#relop a = 1 == 2")
    env.step
    assertEquals(BooleanValue(false), env.state.get(prefix + "a"))
  }

  @Test
  def stackAllocateInst = {
    val (module, env) = prepareCode("#stack a : #int32*")
    env.step
    val a = env.state.get(prefix + "a").asInstanceOf[ScalarReference]
    assertEquals(Int32Value(0), a.value)
  }

  @Test
  def stackAllocateArrayInst = {
    val (module, env) = prepareCode("#stackarray a = 12L * #unit")
    env.step
    val value = env.state.get(prefix + "a")
    assertTrue(value.isInstanceOf[ScalarReference])
    val arrayValue = value.asInstanceOf[ScalarReference].value
    assertTrue(arrayValue.isInstanceOf[ArrayValue])
    val array = arrayValue.asInstanceOf[ArrayValue].value
    assertEquals(12, array.length)
  }

  @Test
  def staticCallAndReturnInst = {
    val program = "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #scall i1 = f( )\n" +
                  "    #return r = ()\n" +
                  "  }\n" +
                  "}\n\n" +
                  "#function f( ): #int32 {\n" +
                  "  #block entry( ) {\n" +
                  "    #return r = 12\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step // call
    val r = module.getInstruction("f.entry.r")
    assertEquals(r, env.state.inst)
    val stackSize = env.stack.size
    env.step // return
    assertEquals(stackSize - 1, env.stack.size)
    assertEquals(Int32Value(12), env.state.get("main.entry.i1"))
  }

  @Test
  def storeInst = {
    val code = "#stack a : #int32*\n" +
               "#store b = *a <- 12"
    val (module, env) = prepareCode(code)
    env.step
    env.step
    assertEquals(Int32Value(12), env.state.get(prefix + "a").asInstanceOf[ScalarReference].value)
    assertEquals(UnitValue, env.state.get(prefix + "b"))
  }

  @Test
  def storeArrayInst = {
    val code = "#stack a : [1 * #int32]*\n" +
               "#address b = a, 0L\n" +
               "#store *b <- 12"
    val (module, env) = prepareCode(code)
    for (i <- 0 until 3) env.step
    val a = env.state.get(prefix + "a").asInstanceOf[ScalarReference]
    val a0 = a.value.asInstanceOf[ArrayValue].value(0)
    assertEquals(Int32Value(12), a0)
  }

  @Test
  def storeElementInst = {
    val code = "#assign a = [#int32: 12, 34]\n" +
               "#storeelement a, 0L <- 56"
    val (module, env) = prepareCode(code)
    env.step
    env.step
    val a = env.state.get(prefix + "a")
    assertTrue(a.isInstanceOf[ArrayValue])
    assertEquals(Int32Value(56), a.asInstanceOf[ArrayValue].value(0))
  }

  @Test
  def storeElementArrayInst = {
    val program = "#struct A {\n" +
                  "  #field b: #int32\n" +
                  "}\n" +
                  "#function main( ): #unit {\n" +
                  "  #block entry( ) {\n" +
                  "    #assign s = {A: 12}\n" +
                  "    #storeelement s, 0L <- 34\n" +
                  "    #return ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    env.step
    env.step
    val s = env.state.get(prefix + "s")
    assertTrue(s.isInstanceOf[StructValue])
    assertEquals(Int32Value(34), s.asInstanceOf[StructValue].value(0))
  }

  @Test
  def upcastInst = {
    val (module, env) = prepareCode("#upcast a = #null : #null")
    env.step
    assertEquals(NullReference, env.state.get(prefix + "a"))
  }

  @Test
  def addressEquality = {
    val code = "#stack a: [1 * #int32]*\n" +
               "#address b = a, 0L\n" +
               "#address c = a, 0L\n" +
               "#relop d = b == c\n"
    val (module, env) = prepareCode(code)
    for (i <- 0 until 4) env.step
    assertEquals(BooleanValue(true), env.state.get(prefix + "d"))
  }
}
