package tungsten

import Utilities._

final class Struct(name: Symbol,
                   val fields: List[Symbol],
                   location: Location = Nowhere)
  extends Definition(name, location)
{
  def validate(module: Module) = {
    if (fields.isEmpty)
      List(EmptyStructException(name, location))
    else
      fields.flatMap(validateComponent[Field](module, _))
  }

  override def toString = {
    "struct " + name + fields.mkString("{\n  ", "\n  ", "\n}")
  }
}
