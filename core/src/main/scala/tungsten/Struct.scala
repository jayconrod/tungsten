package tungsten

import Utilities._

final case class Struct(override name: Symbol,
                        fields: List[Symbol],
                        override location: Location = Nowhere)
  extends Definition(name, location)
{
  def validateComponents(module: Module) = {
    validateComponentsOfClass[Field](module, fields)
  }

  def validate(module: Module) = Nil

  override def equals(that: Any) = {
    that match {
      case Struct(n, fs, _) if name == n && fields == fs => true
      case _ => false
    }
  }

  override def hashCode = hash("Struct", name, fields)

  override def toString = {
    "#struct " + name + fields.mkString("{\n  ", "\n  ", "\n}")
  }
}
