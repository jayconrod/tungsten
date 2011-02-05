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
    def validateBound(bound: Option[Type], isUpper: Boolean) = {
      bound match {
        case None => Nil
        case Some(ty: ObjectType) => Nil
        case Some(_) => List(TypeParameterInvalidBoundException(name, isUpper, getLocation))
      }
    }

    def validateBoundSubtype = {
      (upperBound, lowerBound) match {
        case (Some(upperBoundType), Some(lowerBoundType)) => {
          if (!lowerBoundType.isSubtypeOf(upperBoundType, module))
            List(TypeParameterBoundsSubtypeException(name, upperBoundType, lowerBoundType, getLocation))
          else
            Nil
        }
        case _ => Nil
      }
    }

    def validateVariance = {
      upperBound match {
        case None => Nil
        case Some(u) => u.validateVariance(Variance.CONTRAVARIANT, module, getLocation)
      }
    }

    super.validate(module) ++
      validateBound(upperBound, true) ++ validateBound(lowerBound, false) ++
      validateBoundSubtype ++
      validateVariance
  }

  def getUpperBoundType(module: Module): ObjectDefinitionType = {
    upperBound match {
      case None => module.rootClassType
      case Some(VariableType(varName)) => {
        val upperBoundParameter = module.getTypeParameter(varName)
        upperBoundParameter.getUpperBoundType(module)
      }
      case Some(ty: ObjectDefinitionType) => ty
      case _ => throw new RuntimeException("invalid type parameter")
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

  def boundsMatch(other: TypeParameter): Boolean = {
    upperBound == other.upperBound && lowerBound == other.lowerBound
  }
}

case class Variance(prefix: String, varianceString: String) {
  import Variance._

  def opposite: Variance = {
    this match {
      case COVARIANT => CONTRAVARIANT
      case CONTRAVARIANT => COVARIANT
      case INVARIANT => INVARIANT
      case _ => throw new RuntimeException("invalid variance")
    }
  }

  override def toString = prefix
}
object Variance {
  val COVARIANT = Variance("+", "covariant")
  val CONTRAVARIANT = Variance("-", "contravariant")
  val INVARIANT = Variance("", "invariant")
}
