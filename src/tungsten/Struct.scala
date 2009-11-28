package tungsten

import Utilities._

final class Struct(name: Symbol,
                   val fields: List[Symbol],
                   location: Location = Nowhere)
  extends Definition(name, location)
{
  def validateComponents(module: Module) = {
    validateComponentsOfClass[Field](module, fields)
  }

  def validate(module: Module) = Nil

  override def toString = {
    "struct " + name + fields.mkString("{\n  ", "\n  ", "\n}")
  }
}
