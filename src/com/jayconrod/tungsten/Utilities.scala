package com.jayconrod.tungsten

private object Utilities {
  def hash(code: Int, x: Any) = {
    val c = if (x == null) 0 else x.hashCode
    c * hashA + hashB
  }
  val hashA = 17
  val hashB = 37

  def isPowerOf2(x: Int) = (x & (x - 1)) == 0

  def joinStrings(separator: String, objects: Iterable[Any]) = {
    val strings = objects.map(_.toString)
    val builder = new StringBuilder
    val i = strings.iterator
    while (i.hasNext) {
      builder.append(i.next)
      if (i.hasNext)
        builder.append(separator)
    }
    builder.toString
  }
}
