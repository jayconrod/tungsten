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

import org.junit.Test
import org.junit.Assert._
import Utilities._

class ElementInstructionTest {
  object dummy
    extends ExtendedInstruction
    with PointerElementInstruction
  {
    def ty = UnitType
    def base = UnitValue
    def operands = Nil
    def indices = Nil
    def name = Symbol("dummy")
    def annotations = Nil
  }

  val program = "is64bit: true\n" +
                "struct @A { field [2 x int64] %a }\n" +
                "class @R {\n" +
                "  field int64 %x\n" +
                "}\n"
  val module = compileString(program)

  val baseType = StructType("A")
  val pointerType = PointerType(baseType)
  val classType = ClassType("R")

  def containsError[T <: CompileException](errors: List[CompileException])(implicit m: Manifest[T]) {
    assertTrue(errors.exists(m.erasure.isInstance _))
  }

  @Test
  def getElementType {
    val indices = List(IntValue(0, 64), IntValue(1, 64))
    val elementType = dummy.getElementType(module, baseType, indices)
    assertEquals(IntType(64), elementType)
  }

  @Test
  def getPointerType {
    val indices = List(IntValue(2, 64), IntValue(0, 64), IntValue(1, 64))
    val elementType = dummy.getPointerType(module, pointerType, indices)
    assertEquals(PointerType(IntType(64)), elementType)
  }

  @Test
  def getPointerTypeClass {
    val indices = List(IntValue(0, 64))
    val elementType = dummy.getPointerType(module, classType, indices)
    assertEquals(PointerType(IntType(64)), elementType)
  }

  @Test
  def validateIndicesCorrect {
    val indices = List(IntValue(0, 64), IntValue(1, 64))
    assertEquals(Nil, dummy.validateIndices(module, baseType, indices))
  }

  @Test
  def validateIndicesBadIndex {
    val indices = List(IntValue(0, 32))
    containsError[TypeMismatchException](dummy.validateIndices(module, baseType, indices))
  }

  @Test
  def validateIndicesOutOfRange {
    val indices = List(IntValue(1, 64))
    containsError[InvalidIndexException](dummy.validateIndices(module, baseType, indices))
  }

  @Test
  def validateIndicesNonConstant {
    val indices = List(DefinedValue("a", IntType(64)))
    containsError[InvalidIndexException](dummy.validateIndices(module, baseType, indices))
  }

  @Test
  def validateIndicesNonAggregate {
    val indices = List(IntValue(0, 64))
    val baseType = IntType(64)
    containsError[InvalidIndexException](dummy.validateIndices(module, baseType, indices))
  }

  @Test
  def validatePointerIndicesNonPointer {
    val indices = List(IntValue(0, 64))
    containsError[TypeMismatchException](dummy.validatePointerIndices(module, baseType, indices))
  }

  @Test
  def validatePointerIndicesNil {
    containsError[MissingElementIndexException](dummy.validatePointerIndices(module, pointerType, Nil))
  }

  @Test
  def validatePointerIndicesClassArray {
    val indices = List(IntValue(0, 64), IntValue(0, 64))
    containsError[InvalidIndexException](dummy.validatePointerIndices(module, classType, indices))
  }

  @Test
  def validatePointerIndicesClassNonConstant {
    val indices = List(DefinedValue("x", IntType(64)))
    containsError[InvalidIndexException](dummy.validatePointerIndices(module, classType, indices))
  }

  @Test
  def validatePointerIndicesClassNegative {
    val indices = List(IntValue(-1, 64))
    containsError[InvalidIndexException](dummy.validatePointerIndices(module, classType, indices))
  }

  @Test
  def validatePointerIndicesClassTooBig {
    val indices = List(IntValue(1, 64))
    containsError[InvalidIndexException](dummy.validatePointerIndices(module, classType, indices))
  }
}

