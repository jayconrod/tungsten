package tungsten.interpreter

import org.junit.Test
import org.junit.Assert._
import tungsten._

class EnvironmentTest {
  val ret = ReturnInstruction(new Symbol(List("main", "entry", "ret")), tungsten.UnitValue())
  val entryBlock = Block(new Symbol(List("main", "entry")), Nil, List(ret.name))
  val entry = new Function(new Symbol("main"),
                           Nil,
                           Nil,
                           UnitType(),
                           List(entryBlock.name))
  val module = new Module
  module.add(ret)
  module.add(entryBlock)
  module.add(entry)

  val env = new Environment(module)

  @Test
  def blockIp = {
    val expected = module.get[Instruction](new Symbol(List("main", "entry", "ret"))).get
    assertEquals(expected, env.blockIp(entryBlock).head)
  }

  @Test
  def setArguments = {
    val names = List("foo", "bar", "baz").map(new Symbol(_))
    val args = List(1, 2, 3).map(Int32Value(_))
    env.setArguments(names, args)
    for ((n, a) <- names zip args)
      assertEquals(a, env.state.values(n))
  }
}
