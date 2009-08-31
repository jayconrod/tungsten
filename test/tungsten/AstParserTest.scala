package tungsten

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

class AstParserTest {
  @Ignore @Test
  def empty = {
    assertEquals(AstModule(Nil), AstParser.parse("").get)
  }

  @Ignore @Test
  def whitespace = {
    val program = " \t\n"
    val expected = AstModule(Nil)
    assertEquals(expected, AstParser.parse(program).get)
  }

  @Ignore @Test
  def global = {
    val program = "#global foo: #unit"
    val global = AstGlobal(new Symbol("foo"),
                           AstUnitType(Nowhere),
                           None,
                           Nowhere)
    val expected = AstModule(List(global))
    assertEquals(expected, AstParser.parse(program).get)
  }
}
