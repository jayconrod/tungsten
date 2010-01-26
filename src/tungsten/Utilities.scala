package tungsten

import java.io._

object Utilities {
  val ERROR_CODE = 127
  val FAILURE_CODE = 1

  def compileString(program: String): Module = {
    val ast = AstParser.test(program)
    ast.compile.left.get
  }

  def exitWithError(message: String) {
    System.err.println(message)
    System.exit(ERROR_CODE)
  }

  def exitWithFailure(message: String) {
    System.err.println(message)
    System.exit(FAILURE_CODE)
  }

  def fileWithExtension(file: File, oldExt: String, newExt: String): File = {
    val oldFilename = file.getCanonicalPath
    val newFilename = if (oldFilename.endsWith(oldExt))
      oldFilename.substring(0, oldFilename.length - oldExt.length) + newExt
    else
      oldFilename + newExt
    new File(newFilename)
  }

  def hash(code: Int, x: Any): Int = {
    val hashA = 17
    val hashB = 37
    val c = if (x == null) 0 else x.hashCode
    c * hashA + hashB
  }
  def hash(elements: Any*): Int = elements.foldLeft(0)(hash _)

  def humanReadableClassName[T <: Definition](implicit m: Manifest[T]) = {
    val className = m.toString.split("\\.").last
    className.charAt(0).toLower + className.tail.map({c =>
      if (c.isUpper) " " + c.toLower else c.toString
    }).mkString
  }

  def isPowerOf2(x: Int) = (x & (x - 1)) == 0

  def readContentsOfFile(file: File): String = {
    val reader = new FileReader(file)
    val contents = readContentsOfFile(reader)
    reader.close
    contents
  }

  def readContentsOfFile(input: Reader): String = {
    val buffer = new StringBuffer
    val block = new Array[Char](4096)
    var count = input.read(block)
    while (count != -1) {
      for (i <- 0 until count)
        buffer.append(block(i))
      count = input.read(block)
    }
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

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T], 
               e: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T], h: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g, h)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T], h: => List[T],
               i: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g, h, i)
  }

  def stage[T](a: List[T], b: => List[T], c: => List[T], d: => List[T],
               e: => List[T], f: => List[T], g: => List[T], h: => List[T],
               i: => List[T], j: => List[T]): List[T] =
  {
    if (!a.isEmpty) a else stage(b, c, d, e, f, g, h, i, j)
  }

  implicit def symbolFromString(string: String) = {
    import scala.util.parsing.input.CharArrayReader
    val reader = new CharArrayReader(string.toCharArray)
    val result = AstLexer.phrase(AstLexer.symbol)(reader)
    result match {
      case AstLexer.Success(sym, _) => sym
      case _ => throw new IllegalArgumentException
    }
  }
}
