package tungsten

import Utilities._

final case class Struct(override name: Symbol,
                        fields: List[Symbol],
                        override annotations: List[AnnotationValue] = Nil,
                        override location: Location = Nowhere)
  extends Definition(name, annotations, location)
{
  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Field](module, fields)
  }

  override def equals(that: Any) = {
    that match {
      case Struct(n, fs, as, _) if name == n && fields == fs && annotations == as => true
      case _ => false
    }
  }

  override def hashCode = hash("Struct", name, fields)
}
