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

import java.lang.reflect.Method
import Utilities._

object TestUtilities {
  def assertDefnEquals(name: Symbol, actual: AnyRef, expected: AnyRef) {
    def compare(left: Any, 
                right: Any, 
                fieldNames: List[String]): Option[(List[String], Any, Any)] = 
    {
      def comparePrimitives(left: Any, right: Any) = {
        if (left == right)
          None
        else
          Some((fieldNames, left, right))
      }

      def compareLists(left: List[_], right: List[_], index: Int): Option[(List[String], Any, Any)] = {
        (left, right) match {
          case (Nil, Nil) => None
          case (l :: ls, r :: rs) => {
            val cmp = compare(l, r, index.toString :: fieldNames)
            if (cmp.isDefined) cmp else compareLists(ls, rs, index + 1)
          }
          case _ => Some((fieldNames, left, right))
        }
      }

      def compareOptions(left: Option[_], right: Option[_]): Option[(List[String], Any, Any)] = {
        (left, right) match {
          case (None, None) => None
          case (Some(l), Some(r)) => compare(l, r, fieldNames)
          case _ => Some((fieldNames, left, right))
        }
      }

      def compareFields(getters: Seq[Method]): Option[(List[String], Any, Any)] = {
        import java.lang.reflect.Modifier._
        getters.toList match {
          case Nil => None
          case f :: fs => {
            val fieldName = f.getName
            val cmp = compare(f.invoke(left), f.invoke(right), fieldName :: fieldNames)
            if (cmp.isDefined) cmp else compareFields(fs)
          }
        }
      }

      left match {
        case _: Int | _: Long | _: Double | _: String | _: Symbol =>
          comparePrimitives(left, right)
        case _: List[_] => {
          val l = left.asInstanceOf[List[AnyRef]]
          val r = right.asInstanceOf[List[AnyRef]]
          compareLists(l, r, 0)
        }
        case _: Option[_] => {
          val l = left.asInstanceOf[Option[AnyRef]]
          val r = right.asInstanceOf[Option[AnyRef]]
          compareOptions(l, r)
        }
        case _ => {
          val (l, r) = (left.asInstanceOf[AnyRef], right.asInstanceOf[AnyRef])
          assert(!left.isInstanceOf[List[_]])
          if (l.getClass != r.getClass)
            Some((fieldNames, left, right))
          else {
            val clas = left.asInstanceOf[AnyRef].getClass
            val constructor = clas.getDeclaredConstructors.head
            val argumentCount = constructor.getParameterTypes.size
            if (argumentCount == 0)
              comparePrimitives(left, right)
            else {
              val fields = clas.getDeclaredFields
              val getters = fields.map { f => clas.getMethod(f.getName) }
              compareFields(getters)
            }
          }
        }
      }
    }

    compare(actual, expected, Nil) match {
      case Some((fieldNames, left, right)) => {
        throw new RuntimeException("%s: in field %s, expected <<<%s>>> but was <<<%s>>>".
                                   format(name, fieldNames.reverse.mkString("."), left, right))
      }
      case _ => ()
    }
  }

  def assertSymbolsEqual(expected: Traversable[Symbol], actual: Traversable[Symbol]) {
    assertDefnEquals("unknown", expected.map(nullifySymbol _), actual.map(nullifySymbol _))
  }

  def assertDefnEqualsIgnoreSymbols(expected: Definition, actual: Definition) {
    assertDefnEquals(nullifySymbol(expected.name),
                     expected.mapSymbols(nullifySymbol _),
                     actual.mapSymbols(nullifySymbol _))
  }

  def assertEqualsIgnoreSymbols(expected: Module, actual: Module) {
    expected.definitions.values foreach { expectedDefn =>
      val name = nullifySymbol(expectedDefn.name)
      val matchingDefns = actual.definitions.values.filter { defn =>
        nullifySymbol(defn.name) == name
      }
      matchingDefns match {
        case actualDefn :: Nil if expectedDefn.getClass == actualDefn.getClass => 
          assertDefnEqualsIgnoreSymbols(expectedDefn, actualDefn)
        case actualDefn :: Nil => {
          throw new RuntimeException("for definition %s, expected %s but was %s".
                                     format(expectedDefn.name, 
                                            expectedDefn.getClass.getName,
                                            actualDefn.getClass.getName))
        }
        case Nil => throw new RuntimeException("no matching definitions for symbol: " + expectedDefn.name)
        case _ => ()  // ignore multiple definitions with same name
      }
    }
  }

  def nullifySymbol(sym: Symbol): Symbol = {
    sym.copy(id = 0)
  }
}
