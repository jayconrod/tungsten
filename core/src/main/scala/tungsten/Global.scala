package tungsten

import Utilities._

final case class Global(name: Symbol,
                        ty: Type,
                        value: Option[Value],
                        annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  def ty(module: Module): Type = ty

  override def validate(module: Module) = {
    def validateValueLiteral = {
      value match {
        case Some(_: DefinedValue) => List(GlobalValueNonLiteralException(name, getLocation))
        case _ => Nil
      }
    }

    stage(super.validate(module),
          validateValueLiteral,
          value.toList.flatMap { v => checkType(v.ty, ty, getLocation) })
  }
}
