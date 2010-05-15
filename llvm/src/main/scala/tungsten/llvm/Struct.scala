package tungsten.llvm

final case class Struct(override name: String, fieldTypes: List[Type])
  extends Definition(name)
{
  override def toString = "%s = type %s".format(name, fieldTypes.mkString("{", ", ", "}"))
}
