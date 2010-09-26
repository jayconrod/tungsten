package tungsten

import Utilities._

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

object TypeParameter {
  val defaultUpperBound: Type = ClassType("tungsten.Object")
  val defaultLowerBound: Type = ClassType("tungsten.Nothing")
}
