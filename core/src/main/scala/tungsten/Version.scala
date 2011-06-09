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

final case class Version(elements: List[Int]) 
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

  override def toString = elements.mkString(".")
}

object Version {
  def apply(elements: Int*) = new Version(elements.toList)

  val MAX = new Version(List(1000000))
  val MIN = new Version(Nil)
}
