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

class MiscDefinitionValidationTest
  extends ValidationTest
{
  @Test
  def globalUse {
    val program = "is64bit: true\n" +
                  "global unit @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    unit* %a = address unit* @foo, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programIsCorrect(program)
  }

  @Test
  def nonLiteralGlobal {
    val (foo, bar) = (Symbol("foo"), Symbol("bar"))
    val gfoo = Global(foo, UnitType, Some(UnitValue))
    val gbar = Global(bar, UnitType, Some(DefinedValue(foo, UnitType)))
    val module = (new Module).add(gfoo, gbar)
    containsError[GlobalValueNonLiteralException](gbar.validate(module))
  }

  @Test
  def globalTypeMismatch {
    val program = "global unit @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    store unit* @foo, int32 12\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def duplicateStructField {
    val field = Field("foo", UnitType)
    val struct = Struct("bar", List(field.name, field.name))
    val module = (new Module).add(field, struct)
    val errors = struct.validateComponents(module)
    containsError[DuplicateComponentException](errors)
  }

  @Test
  def singleCyclicStruct {
    val program = "struct @A { field struct @A %x }\n" +
                  "function unit @main { block %entry { return () } }\n"
    programContainsError[CyclicStructException](program)
  }

  @Test
  def doubleCyclicStruct {
    val program = "struct @A { field struct @B %x }\n" +
                  "struct @B { field struct @A %y }\n" +
                  "function unit @main { block %entry { return () } }\n"
    programContainsError[CyclicStructException](program)
  }

  @Test
  def duplicateDependency {
    val module = new Module(dependencies = List(ModuleDependency("a", Version.MIN, Version.MAX),
                                                ModuleDependency("a", Version.MIN, Version.MAX)))
    val errors = module.validate
    containsError[DuplicateDependencyException](errors)
  }

  @Test
  def nonExistantAnnotation {
    val program = "@foo global unit @bar"
    programContainsError[UndefinedSymbolException](program)
  }

  @Test
  def annotationFieldCount {
    val program = "annotation @foo(unit %a)\n" +
                  "@foo global unit @bar"
    programContainsError[AnnotationArgumentCountException](program)
  }

  @Test
  def annotationFieldType {
    val program = "annotation @foo(unit %a)\n" +
                  "@foo(true) global unit @bar"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def validateAnnotationScope {
    val program = "annotation @A(type @T %x)\n" +
                  "class @R\n" + 
                  "function unit @f[type @T]\n"
    programContainsError[ScopeException](program)
  }

  @Test
  def validateStructScope {
    val program = "struct @S {\n" +
                  "  field type @T %x\n" +
                  "}\n" +
                  "class @R\n" +
                  "function unit @f[type @T]\n"
    programContainsError[ScopeException](program)
  }

  @Test
  def validateGlobalScope {
    val program = "global type @T @g\n" +
                  "class @R\n" +
                  "function unit @f[type @T]\n"
    programContainsError[ScopeException](program)
  }

  @Test
  def definedValueTypeMatchesDefinition {
    val program = "function int32 @f(int64 %x) {\n" +
                  "  block %entry {\n" +
                  "    return int32 @f.x\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidDefinedValueException](program)
  }
}
