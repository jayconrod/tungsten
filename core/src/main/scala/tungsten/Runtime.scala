package tungsten

import Utilities._

object Runtime {
  val code = "name: @tungsten\n" +
             "type: intermediate\n" +
             "version: v0.3\n" +
             "is64bit: true\n" +
             "safe: true\n" +
             "class @tungsten.Object\n" +
             "annotation @tungsten.Location(string %filename,\n" +
             "                              int32 %beginLine,\n" +
             "                              int32 %beginColumn,\n" +
             "                              int32 %endLine,\n" +
             "                              int32 %endColumn)\n"
  lazy val runtime64 = compileString(code)
  lazy val runtime32 = runtime64.copyWith(is64Bit = false)

  def getRuntime(is64Bit: Boolean): Module = if (is64Bit) runtime64 else runtime32
}
