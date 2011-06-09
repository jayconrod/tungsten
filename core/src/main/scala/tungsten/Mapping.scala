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

import java.lang.reflect.{Field => ReflectedField}

/** Mapping uses reflection to manipulate the fields of an object recursively. This is very
 *  useful for updates parts of a large, complicated since you don't have to know anything
 *  about its structure. Classes which mix in this trait are assumed to be global case classes.
 *  They must be case classes since there is expected to be a 1-to-1 mapping between fields
 *  and constructor parameters (so new, updated objects can be built). They must be global
 *  so that there are no implicit constructor parameters (which would violate the 
 *  afforementioned 1-to-1 mapping.
 */
trait Mapping[T <: AnyRef]
{
  /** Create a new object by applying the given function to each field of this object. This
   *  essentially gets a list of fields, maps it with the function, and uses it as a parameter
   *  list for the constructor of a new object. The function is not applied recursively to
   *  any values beyond the fields of this object.
   *  @param function a function which takes the name of the field and its original value 
   *    and returns a new value. The new value must have a valid class for this field.
   *  @return a new object of the same class as this one with updated fields
   */
  def mapFields(function: (ReflectedField, AnyRef) => AnyRef): T = {
    val clas = getClass
    val constructor = clas.getDeclaredConstructors.head    
    val argumentCount = constructor.getParameterTypes.size
    if (argumentCount == 0) 
      this.asInstanceOf[T]
    else {
      val fields = clas.getDeclaredFields
      assert(argumentCount <= fields.size)
      val arguments = for (i <- 0 until argumentCount) yield {
        val fieldName = fields(i).getName
        val getter = clas.getMethod(fieldName)
        val oldValue = getter.invoke(this)
        function(fields(i), oldValue)
      }
      constructor.newInstance(arguments: _*).asInstanceOf[T]
    }
  }

  /** Similar to mapFields but recurses through the tree of objects, applying the given function
   *  to every field of the correct type. This method will recurse not only through values
   *  with this trait, but also through lists. The given function is applied to a value after
   *  its components have been updated (so post-traversal). This object must not have any
   *  cyclic references, as they will through this method into an infinite loop.
   */
  def mapRecursively[S <: AnyRef](function: S => S)(implicit m: Manifest[S]): T = {
    val SClass = m.erasure.asInstanceOf[java.lang.Class[S]]
    def mapper(field: ReflectedField, oldValue: AnyRef): AnyRef = {
      val mapped = oldValue match {
        case v: Mapping[_] => v.mapFields(mapper _)
        case list: List[_] => list.map { e => mapper(null, e.asInstanceOf[AnyRef]) }
        case opt: Option[_] => opt.map { e => mapper(null, e.asInstanceOf[AnyRef]) }
        case _ => oldValue
      }
      if (SClass.isInstance(mapped))
        function(mapped.asInstanceOf[S])
      else
        mapped
    }
    mapper(null, this).asInstanceOf[T]
  }

  def mapSymbols(function: Symbol => Symbol): T = mapRecursively(function)
  def mapValues(function: Value => Value): T = mapRecursively(function)
  def mapTypes(function: Type => Type): T = mapRecursively(function)

  /** Accumulates a value by recursively traversing the given object tree, applying the given
   *  function on each field within its domain. Like mapRecursively, this will recurse through
   *  values with this trait as well as lists. The object tree must not be cyclic. The function
   *  is applied in post-traversal order.
   */
  def foldRecursively[S, V](accum: V, function: (V, S) => V)(implicit m: Manifest[S]): V = {
    val SClass = m.erasure.asInstanceOf[java.lang.Class[S]]
    def fold(accum: V, field: AnyRef): V = {
      val newAccum = field match {
        case v: Mapping[_] => {
          val clas = v.getClass
          val fields = clas.getDeclaredFields
          val fieldNames = fields.map(_.getName)
          val fieldValues = fieldNames.collect { case name if !name.contains('$') =>
            clas.getMethod(name).invoke(v)
          }
          (accum /: fieldValues)(fold _)
        }
        case list: List[_] => (accum /: list) { (a, e) => fold(a, e.asInstanceOf[AnyRef]) }
        case Some(v: AnyRef) => fold(accum, v)
        case _ => accum
      }
      if (SClass.isInstance(field))
        function(newAccum, field.asInstanceOf[S])
      else
        newAccum
    }
    fold(accum, this)
  }

  def foldSymbols[V](accum: V, function: (V, Symbol) => V) = foldRecursively(accum, function)
  def foldValues[V](accum: V, function: (V, Value) => V) = foldRecursively(accum, function)
  def foldTypes[V](accum: V, function: (V, Type) => V) = foldRecursively(accum, function)
}
