package tungsten

import Utilities._

final class Version(val elements: List[Int]) 
  extends Ordered[Version]
{
  if (elements.exists(_ < 0))
    throw new IllegalArgumentException

  def compare(that: Version): Int = {
    def compareElements(v1: List[Int], v2: List[Int]): Int = {
      (v1, v2) match {
        case (Nil, Nil) => 0
        case (_, Nil) => 1
        case (Nil, _) => -1
        case (h1 :: t1, h2 :: t2) => {
          val cmp = h1 - h2
          if (cmp < 0)
            -1
          else if (cmp > 0)
            1
          else
            compareElements(t1, t2)
        }
      }
    }
    compareElements(elements, that.elements)
  }

  override def equals(that: Any) = {
    that match {
      case v: Version if elements == v.elements => true
      case _ => false
    }
  }

  override def hashCode = hash(elements)

  override def toString = elements.mkString(".")
}

object Version {
  val MAX = new Version(List(1000000))
  val MIN = new Version(Nil)
}
