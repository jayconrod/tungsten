package com.jayconrod.tungsten

private object Utilities {
  def hash(code: Int, x: Any) = {
    val c = if (x == null) 0 else x.hashCode
    c * hashA + hashB
  }
  val hashA = 17
  val hashB = 37
}
