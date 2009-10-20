package tungsten

import java.io.File
import java.io.FileReader

private object Utilities {
  def hash(code: Int, x: Any) = {
    val c = if (x == null) 0 else x.hashCode
    c * hashA + hashB
  }
  val hashA = 17
  val hashB = 37

  def isPowerOf2(x: Int) = (x & (x - 1)) == 0

  def readFile(file: File) = {
    val buffer = new StringBuffer
    val reader = new FileReader(file)
    val block = new Array[Char](4096)
    var count = reader.read(block)
    while (count != -1) {
      for (i <- 0 until count)
        buffer.append(block(i))
      count = reader.read(block)
    }
    reader.close
    buffer.toString 
  }

  def compileString(program: String) = {
    val ast = AstParser.test(program)
    ast.compile.left.get
  }
}
