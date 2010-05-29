package tungsten.llvm

final case class Global(override name: String, attributes: List[Attribute], value: Value)
  extends Definition(name)
{
  override def toString = {
    val attribStr = if (attributes.isEmpty) "" else " " + attributes.mkString(" ")
    "%s = %s %s".format(name, attribStr, value.typedToString)
  }
}
