package tungsten.llvm

final case class Global(override val name: String, value: Either[Value, Type])
  extends Definition(name)
{
  override def toString = {
    value match {
      case Left(v) => "%s = global %s".format(name, v.typedToString)
      case Right(t) => "%s = external global %s".format(name, t)
    }
  }
}
