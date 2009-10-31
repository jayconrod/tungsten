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
    def validateValue = {
      value match {
        case Some(DefinedValue(_, _)) => List(GlobalValueNonLiteralException(name, location))
        case _ => Nil
      }
    }

    stage(ty.validate(module), 
          validateValue,
          value.toList.flatMap(_.validateType(module, ty)))
  }

  override def toString = "global " + name + ": " + ty
}
