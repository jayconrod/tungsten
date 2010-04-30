package tungsten

import org.junit.Test
import org.junit.Assert._

case class CopyingFoo(x: String, y: String)
  extends Copying[CopyingFoo]

class CopyingTest {
  @Test
  def copyingTest {
    val a = CopyingFoo("a", "b")
    assertEquals(a, a.copyWith())
    assertEquals(CopyingFoo("c", "b"), a.copyWith(("x", "c")))
  }
}
