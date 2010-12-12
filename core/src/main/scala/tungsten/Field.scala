package tungsten

final case class Field(name: Symbol, 
                       ty: Type, 
                       annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validate(module: Module): List[CompileException] = {
    super.validate(module) ++
      ty.validateVariance(Variance.INVARIANT, module, getLocation)
  }
}

                  
