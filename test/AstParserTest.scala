package tungsten

import org.junit.Test
import org.junit.Assert._

class AstParserTest {
  @Test
  def empty = {
    val expected = AstModule(Nil)
    val result = AstParser.parse("")
    assertEquals(expected, result.get)
  }
}
