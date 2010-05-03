package tungsten

import Utilities._

final case class Global(name: Symbol,
                        ty: Type,
                        value: Option[Value],
                        annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  def ty(module: Module): Type = ty

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      ty.validate(module, getLocation) ++ 
      value.toList.flatMap(_.validateComponents(module, getLocation))
  }

  override def validate(module: Module) = {
    def validateValueLiteral = {
      value match {
        case Some(_: DefinedValue) => List(GlobalValueNonLiteralException(name, getLocation))
        case _ => Nil
      }
    }

    stage(super.validate(module),
          validateValueLiteral,
          value.toList.flatMap { v => 
            v.validate(module, getLocation) ++ v.validateType(ty, getLocation)
          })
  }
}
