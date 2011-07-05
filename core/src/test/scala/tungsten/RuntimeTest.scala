package tungsten

import org.junit.Test
import org.junit.Assert._

class RuntimeTest {
  def testModule(module: Module) {
    val errors = module.validate
    assertEquals(Nil, errors)
  }

  @Test
  def runtime64IsValid {
    testModule(Runtime.getRuntime(true))
  }

  @Test
  def runtime32IsValid {
    testModule(Runtime.getRuntime(false))
  }
}
