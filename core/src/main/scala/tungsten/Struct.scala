package tungsten

import Utilities._

final case class Struct(override name: Symbol,
                        fields: List[Symbol],
                        override annotations: List[AnnotationValue] = Nil)
  extends Definition(name, annotations)
{
  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Field](module, fields)
  }
}
