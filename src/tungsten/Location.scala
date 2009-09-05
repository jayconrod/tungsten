package tungsten

sealed case class Location(val filename: String,
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

  def this(filename: String, line: Int, column: Int) = {
    this(filename, line, column, line, column)
  }

  def combine(loc: Location) = {
    if (filename != loc.filename)
      throw new IllegalArgumentException
    new Location(filename, beginLine, beginColumn, loc.endLine, loc.endColumn)
  }

  final override def equals(that: Any) = {
    that match {
      case Location(fn, bl, bc, el, ec) if filename == fn &&
                                           beginLine == bl &&
                                           beginColumn == bc &&
                                           endLine == el &&
                                           endColumn == ec => true
      case _ => false
    }
  }

  final override def hashCode: Int = {
    val parts = List[Any](filename, beginLine, beginColumn, endLine, endColumn)
    parts.foldLeft(0)(Utilities.hash _)
  }
}

object Nowhere extends Location("<NOWHERE>", 1, 1, 1, 1)

