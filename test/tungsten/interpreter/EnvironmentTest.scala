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
    while (env.state.ip.head != firstInst)
      env.step
    (module, env)
  }

  def prepareCode(code: String) = {
    val program = "#function main( ): #unit { #block entry( ) { " + code + " } }"
    prepare(program)
  }

  @Test
  def blockIp = {
    val program = "#function main( ): #unit { #block entry( ) { #return ret = () } }"
    val (module, env) = prepare(program)
    val block = module.get[Block](new Symbol(List("main", "entry"))).get
    val expected = module.get[Instruction](block.instructions.head).get
    
    assertEquals(expected, env.blockIp(block).head)
  }

  @Test
  def setArguments = {
    val env = new Environment(new Module)
    val names = List("foo", "bar", "baz").map(new Symbol(_))
    val args = List(1, 2, 3).map(Int32Value(_))
    env.setArguments(names, args)
    for ((n, a) <- names zip args)
      assertEquals(a, env.state.values(n))
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
    env.state = new State(env.blockIp(b1))
    env.branch(b2, List(Int32Value(12), Int32Value(34)))
    assertEquals(module.getDefn(b2.instructions.head).get, env.state.ip.head)
    assertEquals(Int32Value(12), env.state.values(new Symbol(List("main", "b2", "x"))))
    assertEquals(Int32Value(34), env.state.values(new Symbol(List("main", "b2", "y"))))
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
    assertEquals(calledInst, env.state.ip.head)
    assertEquals(Int32Value(123), env.state.values(new Symbol(List("f", "x"))))
  }

  @Test
  def assignInst = {
    val (module, env) = prepareCode("#assign a = 123\n#assign b = a")
    env.step
    assertEquals(Int32Value(123), env.state.values(prefix + "a"))
    env.step
    assertEquals(Int32Value(123), env.state.values(prefix + "b"))
  }
}
