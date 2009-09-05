package tungsten

import org.junit.Test
import org.junit.Assert._

class AstParserTest {
  @Test
  def empty = {
    val input = ""
    val expected = AstModule(Nil)
    val program = AstParser.test(input)
    assertEquals(expected, program)
  }

  @Test
  def whitespace = {
    val input = " \t\n"
    val expected = AstModule(Nil)
    val program = AstParser.test(input)
    assertEquals(expected, program)
  }
}
