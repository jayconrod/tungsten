package tungsten

final case class Annotation(override name: Symbol,
                            fields: List[Symbol],
                            override annotations: List[AnnotationValue],
                            override location: Location = Nowhere)
  extends Definition(name, annotations, location)
{
  def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Field](module, fields)
  }

  override def toString = {
    "#annotation " + name + fields.mkString("{\n  ", "\n  ", "\n}")
  }
}

final case class AnnotationValue(name: Symbol, fields: List[Value]) {
  override def toString = {
    val fieldsStr = fields match {
      case Nil => ""
      case _ => fields.mkString("(", ", ", ")")
    }
    "@" + name + fieldsStr
  }
}

