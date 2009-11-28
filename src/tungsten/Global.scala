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

  def validateComponents(module: Module) = {
    ty.validate(module)
  }

  def validate(module: Module) = {
    def validateValueLiteral = {
      value match {
        case Some(DefinedValue(_, _)) => List(GlobalValueNonLiteralException(name, location))
        case _ => Nil
      }
    }

    stage(validateValueLiteral,
          value.toList.flatMap(_.validateType(module, ty)))
  }

  override def toString = "global " + name + ": " + ty
}
