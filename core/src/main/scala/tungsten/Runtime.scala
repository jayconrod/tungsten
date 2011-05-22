package tungsten

import Utilities._

object Runtime {
  lazy val runtime64: Module = {
    val input = getClass.getResourceAsStream("runtime.w")
    val reader = new java.io.InputStreamReader(input)
    ModuleIO.readText(reader, "runtime.w")
  }
  lazy val runtime32 = runtime64.copyWith(is64Bit = false)

  def getRuntime(is64Bit: Boolean): Module = if (is64Bit) runtime64 else runtime32
}
