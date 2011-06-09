/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

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
