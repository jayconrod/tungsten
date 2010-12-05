package tungsten

import Utilities._

final case class TypeParameter(name: Symbol,
                               upperBound: Option[Type],
                               lowerBound: Option[Type],
                               variance: Variance,
                               annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def validate(module: Module): List[CompileException] = {
    (upperBound, lowerBound) match {
      case (Some(upperBoundType), Some(lowerBoundType)) => {
        if (!lowerBoundType.isSubtypeOf(upperBoundType, module))
          List(TypeParameterBoundsException(name, upperBoundType, lowerBoundType, getLocation))
        else
          Nil
      }
      case _ => Nil
    }
  }

  def isArgumentInBounds(ty: Type, module: Module): Boolean = {
    val belowUpperBound = upperBound match {
      case Some(upper) => ty.isSubtypeOf(upper, module)
      case None => true
    }
    val aboveLowerBound = lowerBound match {
      case Some(lower) => lower.isSubtypeOf(ty, module)
      case None => true
    }
    belowUpperBound && aboveLowerBound
  }
}

case class Variance(prefix: String) {
  override def toString = prefix
}
object Variance {
  val COVARIANT = Variance("+")
  val CONTRAVARIANT = Variance("-")
  val INVARIANT = Variance("")
}
