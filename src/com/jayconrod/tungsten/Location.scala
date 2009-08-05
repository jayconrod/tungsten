package com.jayconrod.tungsten

final case class Location(val filename: String,
                          val beginLine: Int,
                          val beginColumn: Int,
                          val endLine: Int,
                          val endColumn: Int)
{
  if (filename.isEmpty ||
      beginLine < 1 || beginColumn < 1 || endLine < 1 || endColumn < 1 ||
      (beginLine > endLine) || (beginLine == endLine && beginColumn > endColumn))
  {
    throw new IllegalArgumentException
  }
}

  
