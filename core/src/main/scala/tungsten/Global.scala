package tungsten

import Utilities._

final case class Global(override name: Symbol,
                        ty: Type,
                        value: Option[Value],
                        override annotations: List[AnnotationValue] = Nil,
                        override location: Location = Nowhere)
  extends Definition(name, annotations, location)
{
  def ty(module: Module): Type = ty

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module) ++ 
      value.toList.flatMap(_.validateComponents(module))
  }

  override def validate(module: Module) = {
    def validateValueLiteral = {
      value match {
        case Some(DefinedValue(_, _)) => List(GlobalValueNonLiteralException(name, location))
        case _ => Nil
      }
    }

    stage(super.validate(module),
          validateValueLiteral,
          value.toList.flatMap(_.validateType(ty, module)))
  }
}
