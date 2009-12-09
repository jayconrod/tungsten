package tungsten.interpreter

import org.junit.Test
import org.junit.Assert._
import tungsten._
import tungsten.Utilities._

class EnvironmentTest {
  val prefix = new Symbol(List("main", "entry"))
      
  def prepare(program: String) = {
    val module = compileString(program)
    val env = new Environment(module)
    val mainFunction = module.get[Function](new Symbol("main")).get
    val entryBlock = module.get[Block](mainFunction.blocks.head).get
    val firstInst = module.get[Instruction](entryBlock.instructions.head).get
    while (env.state.inst != firstInst)
      env.step
    (module, env)
  }

  def prepareCode(code: String) = {
    val program = "#function main( ): #unit { #block entry( ) { " + code + " } }"
    prepare(program)
  }

  @Test
  def setArguments = {
    val env = new Environment(new Module)
    val names = List("foo", "bar", "baz").map(new Symbol(_))
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
    val b1 = module.get[Block](new Symbol(List("main", "b1"))).get
    val b2 = module.get[Block](new Symbol(List("main", "b2"))).get
    env.branch(b2, List(Int32Value(12), Int32Value(34)))
    assertEquals(module.getDefn(b2.instructions.head).get, env.state.inst)
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("main", "b2", "x"))))
    assertEquals(Int32Value(34), env.state.get(new Symbol(List("main", "b2", "y"))))
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
    val x = env.state.get(new Symbol(List("f", "x")))
    assertEquals(Int32Value(12), x)
  }

  @Test
  def call = {
    val program = "#function main( ): #unit { #block entry( ) { #return r = () } }\n" +
                  "#function f(x: #int32): #unit { #block entry( ) { #return r = () } }\n"
    val (module, env) = prepare(program)
    val calledFunction = module.get[Function](new Symbol("f")).get
    val calledInst = module.get[Instruction](new Symbol(List("f", "entry", "r"))).get
    val oldSize = env.stack.size
    env.call(calledFunction, List(Int32Value(123)))
    assertEquals(oldSize + 1, env.stack.size)
    assertEquals(calledInst, env.state.inst)
    assertEquals(Int32Value(123), env.state.get(new Symbol(List("f", "x"))))
  }

  @Test
  def addressInst = {
    val code = "#stackarray a = 2L * [2 * #int32]\n" +
               "#address b = a, 1L, 1L"
    val (module, env) = prepareCode(code)
    env.step
    env.step

    val asym = new Symbol(List("main", "entry", "a"))
    val a = env.state.get(asym).asInstanceOf[ScalarReference]
    val aArray1 = a.value.asInstanceOf[ArrayValue]
    val aArray2 = aArray1.value(1).asInstanceOf[ArrayValue]

    val bsym = new Symbol(List("main", "entry", "b"))
    val b = env.state.get(bsym).asInstanceOf[ArrayIndexReference]
    assertSame(aArray2, b.array)
    assertEquals(Int64Value(1), b.index)
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
    val r2 = module.get[Instruction](new Symbol(List("main", "b2", "r2"))).get
    assertEquals(r2, env.state.inst)
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("main", "b2", "x"))))
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
    val barInst = module.get[Instruction](new Symbol(List("main", "bar", "r"))).get
    assertEquals(barInst, env.state.inst)
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("main", "bar", "x"))))
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
    val foo = env.globalState(new Symbol("foo"))
    assertEquals(Int32Value(12), foo.asInstanceOf[ScalarReference].value)
    env.step
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("main", "entry", "a"))))
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
    val foo = env.globalState(new Symbol("foo"))
    assertEquals(Int32Value(12), foo.asInstanceOf[ScalarReference].value)
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
    val f = module.get[Function](new Symbol("f")).get
    assertEquals(FunctionValue(f), env.state.get(new Symbol(List("main", "entry", "g"))))
    env.step    // call
    val r = module.get[Instruction](new Symbol(List("f", "entry", "r"))).get
    assertEquals(r, env.state.inst)
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("f", "x"))))
  }

  @Test
  def loadInst = {
    val code = "#stack a : #int32*\n" +
               "#load b = *a"
    val (module, env) = prepareCode(code)
    env.step
    env.step
    val a = new Symbol(List("main", "entry", "b"))
    assertEquals(Int32Value(0), env.state.get(a))
  }

  @Test
  def loadElementInst = {
    val code = "#loadelement a = [#int32: 12, 34], 0L"
    val (module, env) = prepareCode(code)
    env.step
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("main", "entry", "a"))))
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
    val value = env.state.get(new Symbol(List("main", "entry", "a")))
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
    val r = module.get[Instruction](new Symbol(List("f", "entry", "r"))).get
    assertEquals(r, env.state.inst)
    val stackSize = env.stack.size
    env.step // return
    assertEquals(stackSize - 1, env.stack.size)
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("main", "entry", "i1"))))
  }

  @Test
  def storeInst = {
    val code = "#stack a : #int32*\n" +
               "#store b = *a <- 12"
    val (module, env) = prepareCode(code)
    env.step
    env.step
    val a = new Symbol(List("main", "entry", "a"))
    val b = new Symbol(List("main", "entry", "b"))
    assertEquals(Int32Value(12), env.state.get(a).asInstanceOf[ScalarReference].value)
    assertEquals(UnitValue, env.state.get(b))
  }

  @Test
  def storeElementInst = {
    val code = "#assign a = [#int32: 12, 34]\n" +
               "#storeelement a, 0L <- 56"
    val (module, env) = prepareCode(code)
    env.step
    env.step
    val a = env.state.get(new Symbol(List("main", "entry", "a")))
    assertTrue(a.isInstanceOf[ArrayValue])
    assertEquals(Int32Value(56), a.asInstanceOf[ArrayValue].value(0))
  }

  @Test
  def upcastInst = {
    val (module, env) = prepareCode("#upcast a = #null : #null")
    env.step
    assertEquals(NullReference, env.state.get(prefix + "a"))
  }
}
