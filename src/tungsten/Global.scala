package tungsten

import Utilities._

final case class Global(override name: Symbol,
                        ty: Type,
                        value: Option[Value],
                        override location: Location = Nowhere)
  extends Definition(name, location)
  with TypedDefinition
{
  def ty(module: Module) = ty

  def validate(module: Module) = {
    stage(ty.validate(module), value.toList.flatMap(_.validate(module, ty)))
  }

  override def toString = "global " + name + ": " + ty
}
