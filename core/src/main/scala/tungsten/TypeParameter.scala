package tungsten

final case class TypeParameter(name: Symbol,
                               upperBound: Type,
                               lowerBound: Type,
                               annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validateComponents(module: Module): List[CompileException] = {
    upperBound.validate(module, getLocation) ++
      lowerBound.validate(module, getLocation)
  }

  override def validate(module: Module): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }
}
