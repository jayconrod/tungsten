package tungsten

private object Utilities {
  def hash(code: Int, x: Any) = {
    val c = if (x == null) 0 else x.hashCode
    c * hashA + hashB
  }
  val hashA = 17
  val hashB = 37

  def isPowerOf2(x: Int) = (x & (x - 1)) == 0
}
