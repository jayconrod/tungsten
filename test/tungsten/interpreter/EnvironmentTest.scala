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
  def assignInst = {
    val (module, env) = prepareCode("#assign a = 123\n#assign b = a")
    env.step
    assertEquals(Int32Value(123), env.state.get(prefix + "a"))
    env.step
    assertEquals(Int32Value(123), env.state.get(prefix + "b"))
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
                  "    #gload bar = foo\n" +
                  "    #return r = ()\n" +
                  "  }\n" +
                  "}\n"
    val (module, env) = prepare(program)
    assertEquals(Int32Value(12), env.globalState(new Symbol("foo")))
    env.step
    assertEquals(Int32Value(12), env.state.get(new Symbol(List("main", "entry", "bar"))))
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
}
