package tungsten

import Utilities._

final case class Struct(name: Symbol,
                        fields: List[Symbol],
                        annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Field](module, fields)
  }
}
