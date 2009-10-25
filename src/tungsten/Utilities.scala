package tungsten

import java.io.File
import java.io.FileReader

private object Utilities {
  def compileString(program: String): Module = {
    val ast = AstParser.test(program)
    ast.compile.left.get
  }

  def hash(code: Int, x: Any) = {
    val c = if (x == null) 0 else x.hashCode
    c * hashA + hashB
  }
  val hashA = 17
  val hashB = 37

  def humanReadableClassName[T <: Definition](implicit m: Manifest[T]) = {
    val className = m.toString.split("\\.").last
    className.charAt(0).toLowerCase + className.tail.map({c =>
      if (c.isUpperCase) " " + c.toLowerCase else c.toString
    }).mkString
  }

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

  def stage[T](a: List[T], b: => List[T]): List[T] = {
    if (!a.isEmpty) a else b
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T]): List[T] = {
    if (!a.isEmpty) a else stage(b, c)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T]): List[T] = {
    if (!a.isEmpty) a else stage(b, c, d)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T], e: => List[T]): List[T] = {
    if (!a.isEmpty) a else stage(b, c, d, e)
  }
}
