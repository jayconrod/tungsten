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

import collection.immutable.TreeMap
import org.junit.Test
import org.junit.Assert._
import Utilities._

class ClassValidationTest
  extends ValidationTest
{
  @Test
  def cyclicClassSelfInheritance {
    val program = "class @R\n" +
                  "interface @I <: class @R\n" +
                  "class @A <: class @A {\n" +
                  "  interface @I\n" +
                  "}\n"
    programContainsError[CyclicInheritanceException](program)
  }

  @Test
  def cyclicClassInheritance {
    val program = "class @A <: class @B\n" +
                  "class @B <: class @A\n"
    programContainsError[CyclicInheritanceException](program)
  }

  @Test
  def cyclicInterfaceInheritance {
    val program = "class @A\n" +
                  "interface @B <: interface @C\n" +
                  "interface @C <: class @A {\n" +
                  "  interface @B\n" +
                  "}\n"
    programContainsError[CyclicInheritanceException](program)
  }

  @Test
  def cyclicClassInterfaceInheritance {
    val program = "class @A {\n" +
                  "  interface @I\n" +
                  "}\n" +
                  "class @B <: class @A\n" +
                  "interface @I <: class @B\n"
    programContainsError[CyclicInheritanceException](program)
  }

  @Test
  def cyclicTypeParameter {
    val program = "class @R\n" +
                  "class @A[type @T <: type @S, type @S <: type @T] <: class @R"
    programContainsError[CyclicTypeParameterException](program)
  }

  @Test
  def multipleRootClass {
    val program = "class @A\n" +
                  "class @B\n"
    programContainsError[MultipleRootClassException](program)
  }

  @Test
  def parameterizedRootClass {
    val program = "class @A[type %T]"
    programContainsError[ParameterizedRootClassException](program)
  }

  @Test
  def illegalInterfaceInheritance {
    val program = "class @A\n" +
                  "class @B <: class @A\n" +
                  "interface @I <: class @B\n" +
                  "interface @J <: class @A {\n" +
                  "  interface @I\n" +
                  "}\n"
    programContainsError[IllegalInheritanceException](program)
  }

  @Test
  def illegalClassInheritance {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B <: class @R {\n" +
                  "  interface @I\n" +
                  "}\n" +
                  "interface @I <: class @A\n"
    programContainsError[IllegalInheritanceException](program)
  }

  @Test
  def illegalTypeInheritance { 
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B <: class @R\n" +
                  "class @C[type @T] <: class @R\n" +
                  "interface @I <: class @C[class @A]\n" +
                  "interface @J <: class @C[class @B] {\n" +
                  "  interface @I\n" +
                  "}\n"
    programContainsError[InheritanceConflictException](program)
  }

  @Test
  def conflictingInheritance {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B <: class @R\n" +
                  "interface @I[type @T] <: class @R\n" +
                  "interface @J <: interface @I[class @A]\n" +
                  "interface @K <: interface @J {\n" +
                  "  interface @I[class @B]\n" +
                  "}\n"
    programContainsError[InheritanceConflictException](program)
  }

  @Test
  def inheritedFieldType {
    val program = "class @A {\n" +
                  "  field unit @x\n" +
                  "}\n" +
                  "class @B <: class @A {\n" +
                  "  field boolean @y\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def missingField {
    val program = "class @A { field unit %x }\n" +
                  "class @B <: class @A"
    programContainsError[MissingFieldException](program)
  }

  @Test
  def missingThisParameter {
    val program = "class @A { methods { @f } }\n" +
                  "function unit @f()\n"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  def invalidThisParameter {
    val program = "class @A { methods { @f } }\n" +
                  "function unit @f(int64 %this)"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  def invalidThisParameterTypeArgs {
    val program = "class @R\n" +
                  "class @A[type %T] <: class @R\n" +
                  "class @B[type %T] <: class @R { methods { %f } }\n" +
                  "function unit @B.f[type %T](class @B[class @A[type %T]] %this)"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  def methodTypeParameterMismatch {
    val program = "class @R\n" +
                  "class @A[type %T] <: class @R { methods { %f } }\n" +
                  "function unit @A.f[type %S >: class @R, type %T](class @A[type %T] %this)\n"
    programContainsError[MethodTypeParameterMismatchException](program)
  }

  @Test
  def constructorTypeParameterMismatch {
    val program = "class @R\n" +
                  "class @A[type %T] <: class @R { constructors { %ctor } }\n" +
                  "function unit @A.ctor[type %S >: class @R, type %T](class @A[type %T] %this)\n"
    programContainsError[ConstructorTypeParameterMismatchException](program)
  }

  @Test
  def methodFromUnrelatedClass {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { @A.f } }\n" +
                  "class @B <: class @R { methods { @A.f } }\n" +
                  "function unit @A.f(class @A %this)"
    programContainsError[MethodSelfTypeException](program)
  }

  @Test
  def invalidInheritedMethod {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { @A.f } }\n" +
                  "function unit @A.f(class @R %this)"
    programContainsError[MethodNotInheritedException](program)
  }

  @Test
  def missingMethod {
    val program = "class @A { methods { @A.f } }\n" +
                  "class @B <: class @A\n" +
                  "function unit @A.f(class @A %this)"
    programContainsError[MissingMethodException](program)
  }

  @Test
  def inheritedMethodTypeFromClass {
    val program = "class @A {\n" +
                  "  methods {\n" +
                  "    @A.f\n" +
                  "  }\n" +
                  "}\n" +
                  "class @B <: class @A {\n" +
                  "  methods {\n" +
                  "    @B.f\n" +
                  "  }\n" +
                  "}\n" +
                  "function unit @A.f(class @A %this, unit %x)\n" +
                  "function unit @B.f(class @A %this)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def inheritedMethodTypeFromInterface {
    val program = "class @R\n" +
                  "interface @I <: class @R { methods { @I.f } }\n" +
                  "class @A <: class @R {\n" +
                  "  interface @I { @A.f }\n" +
                  "  methods { @A.f }\n" +
                  "}\n" +
                  "function unit @I.f(interface @I %this, unit %x)\n" +
                  "function unit @A.f(class @A %this)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }
 
  @Test
  def methodVariance {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @C <: class @R { methods { @C.f } }\n" +
                  "class @D <: class @C { methods { @D.f } }\n" +
                  "function class @R @C.f(class @C %this, class @A %x)\n" +
                  "function class @A @D.f(class @D %this, class @R %x)\n"
    programIsCorrect(program)
  }

  @Test
  def constructorNonUnit {
    val program = "class @R { constructors { %ctor } }\n" +
                  "function int64 @R.ctor(class @R %this)\n"
    programContainsError[ConstructorReturnTypeException](program)
  }

  @Test
  def constructorMissingThis {
    val program = "class @R { constructors { %ctor } }\n" +
                  "function unit @R.ctor\n"
    programContainsError[ConstructorSelfTypeException](program)
  }

  @Test
  def constructorWrongThis {
    val program = "class @R\n" +
                  "class @A <: class @R { constructors { %ctor } }\n" +
                  "function unit @A.ctor(class @R %this)"
    programContainsError[ConstructorSelfTypeException](program)
  }

  @Test
  def foreignMethodInInterface {
    val program = "class @R\n" +
                  "interface @I <: class @R { methods { @I.f } }\n" +
                  "class @C <: class @R { interface @I { @I.f } }\n" +
                  "function unit @I.f(class @R %this)\n"
    programContainsError[ForeignInterfaceMethodException](program)
  }

  @Test
  def abstractMethodInNonAbstractClass {
    val program = "annotation @tungsten.Abstract\n" +
                  "class @R { methods { %f } }\n" +
                  "@tungsten.Abstract function unit @R.f(class @R %this)\n"
    programContainsError[AbstractMethodException](program)
  }

  @Test
  def abstractMethodIsDefined {
    val program = "annotation @tungsten.Abstract\n" +
                  "@tungsten.Abstract function unit @f { block %entry { return () } }\n"
    programContainsError[AbstractMethodDefinedException](program)
  }

  @Test
  def finalClassInheritedByClass {
    val program = "annotation @tungsten.Final\n" +
                  "@tungsten.Final class @R\n" +
                  "class @C <: class @R\n"
    programContainsError[FinalClassInheritanceException](program)
  }

  @Test
  def finalClassInheritedByInterface {
    val program = "annotation @tungsten.Final\n" +
                  "@tungsten.Final class @R\n" +
                  "interface @I <: class @R\n"
    programContainsError[FinalClassInheritanceException](program)
  }

  @Test
  def finalMethodOverrideException {
    val program = "annotation @tungsten.Final\n" +
                  "class @R { methods { %f } }\n" +
                  "class @A <: class @R { methods { %f } }\n" +
                  "@tungsten.Final function unit @R.f(class @R %this)\n" +
                  "function unit @A.f(class @A %this)\n"
    programContainsError[FinalMethodOverrideException](program)
  }

  @Test
  def abstractFinalClass {
    val program = "annotation @tungsten.Abstract\n" +
                  "annotation @tungsten.Final\n" +
                  "@tungsten.Abstract @tungsten.Final class @R\n"
    programContainsError[AbstractFinalClassException](program)
  }

  @Test
  def abstractFinalMethod {
    val program = "annotation @tungsten.Abstract\n" +
                  "annotation @tungsten.Final\n" +
                  "@tungsten.Abstract @tungsten.Final function unit @f\n"
    programContainsError[AbstractFinalMethodException](program)
  }

  @Test
  def invalidTypeInClass {
    val program = "class @A <: class @B"
    programContainsError[UndefinedSymbolException](program)
  }

  @Test
  def typeParameterInvalidBounds {
    val program = "function unit @f[type %T <: unit]()"
    programContainsError[TypeParameterInvalidBoundException](program)
  }

  @Test
  def typeParameterBoundsSubtype {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @B[type %T <: class @A >: class @R] <: class @R\n"
    programContainsError[TypeParameterBoundsSubtypeException](program)
  }

  @Test
  def variance {
    val program = "class @R\n" +
                  "class @Pass[type %T] <: class @R\n" +
                  "class @Source[type +%T] <: class @R\n" +
                  "class @Sink[type -%T] <: class @R\n"
    val module = compileString(program)
    def isIncorrect(ty: Type, variance: Variance) {
      val errors = ty.validateVariance(variance, module, Nowhere)
      containsError[TypeParameterVarianceException](errors)
    }
    def isCorrect(ty: Type, variance: Variance) {
      val errors = ty.validateVariance(variance, module, Nowhere)
      assertTrue(errors.isEmpty)
    }

    import Variance._
    isCorrect(VariableType("Pass.T"), INVARIANT)
    isCorrect(VariableType("Pass.T"), COVARIANT)
    isCorrect(VariableType("Pass.T"), CONTRAVARIANT)
    isIncorrect(VariableType("Source.T"), INVARIANT)
    isCorrect(VariableType("Source.T"), COVARIANT)
    isIncorrect(VariableType("Source.T"), CONTRAVARIANT)
    isIncorrect(VariableType("Sink.T"), INVARIANT)
    isIncorrect(VariableType("Sink.T"), COVARIANT)
    isCorrect(VariableType("Sink.T"), CONTRAVARIANT)

    isCorrect(ClassType("Source", List(VariableType("Source.T"))), COVARIANT)
    isIncorrect(ClassType("Source", List(VariableType("Source.T"))), CONTRAVARIANT)
    isCorrect(ClassType("Sink", List(VariableType("Sink.T"))), COVARIANT)
    isIncorrect(ClassType("Sink", List(VariableType("Source.T"))), COVARIANT)
  }

  @Test
  def fieldVariance {
    val program = "class @R\n" +
                  "class @C[type +%T] <: class @R { field type %T %x }"
    programContainsError[TypeParameterVarianceException](program)
  }

  @Test
  def methodResultVariance {
    val program = "function type %T @f[type -%T]()"
    programContainsError[TypeParameterVarianceException](program)
  }

  @Test
  def methodParameterVariance {
    val program = "function unit @f[type +%T](type %T %x)"
    programContainsError[TypeParameterVarianceException](program)
  }

  @Test
  def typeParameterContravariance {
    val program = "class @R\n" +
                  "class @Sink[type -%S] <: class @R\n" +
                  "function class @Sink[type %T] @f[type +%T]()"
    programContainsError[TypeParameterVarianceException](program)
  }

  @Test
  def typeParameterCovariance {
    val program = "class @R\n" +
                  "class @Source[type +%S] <: class @R\n" +
                  "function unit @f[type +%T](class @Source[type %T] %x)"
    programContainsError[TypeParameterVarianceException](program)
  }

  @Test
  def typeParameterUpperBoundVariance {
    val program = "function unit @f[type +%T, type %S <: type %T]()"
    programContainsError[TypeParameterVarianceException](program)
  }

  @Test
  def overrideWrongTypeParameters {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function unit @A.f[type %T](class @A %this)\n" +
                  "function unit @B.f(class @B %this)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def overrideLessTypeParameters {
    val program = "class @R\n" +
                  "class @A[type %T] <: class @R { methods { %f } }\n" +
                  "class @B <: class @A[class @R] { methods { %f } }\n" +
                  "function unit @A.f[type %T, type %S](class @A[type %T] %this, type %S %x, type %T %y)\n" +
                  "function unit @B.f[type %S](class @B %this, type %S %x, class @R %y)\n"
    programIsCorrect(program)
  }

  @Test
  def overrideMoreTypeParameters {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { %f } }\n" +
                  "class @B[type %T] <: class @A { methods { %f } }\n" +
                  "function unit @A.f[type %S](class @A %this, type %S %x)\n" +
                  "function unit @B.f[type %T, type %S](class @B[type %T] %this, type %S %x)\n"
    programIsCorrect(program)
  }

  @Test
  def overrideTypeParameterUpperBoundSuper {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function unit @A.f[type %T <: class @A](class @A %this, type %T %x)\n" +
                  "function unit @B.f[type %T <: class @R](class @B %this, type %T %x)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def overrideTypeParameterUpperBoundSub {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function unit @A.f[type %T <: class @R](class @A %this, type %T %x)\n" +
                  "function unit @B.f[type %T <: class @A](class @B %this, type %T %x)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def overrideTypeParameterLowerBoundSuper {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function unit @A.f[type %T >: class @B](class @A %this, type %T %x)\n" +
                  "function unit @B.f[type %T >: class @A](class @B %this, type %T %x)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def overrideTypeParameterLowerBoundSub {
    val program = "class @R\n" +
                  "class @A <: class @R { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function unit @A.f[type %T >: class @A](class @A %this, type %T %x)\n" +
                  "function unit @B.f[type %T >: class @B](class @B %this, type %T %x)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def overrideContravariantParameters {
    val program = "class @A { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function unit @A.f(class @A %this, class @B %x)\n" +
                  "function unit @B.f(class @B %this, class @A %x)\n"
    programIsCorrect(program)
  }

  @Test
  def overrideCovariantParameters {
    val program = "class @A { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function unit @A.f(class @A %this, class @A %x)\n" +
                  "function unit @B.f(class @B %this, class @B %x)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def overrideContravariantReturn {
    val program = "class @A { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function class @B @A.f(class @A %this)\n" +
                  "function class @A @B.f(class @B %this)\n"
    programContainsError[MethodOverrideCompatibilityException](program)
  }

  @Test
  def overrideCovariantReturn {
    val program = "class @A { methods { %f } }\n" +
                  "class @B <: class @A { methods { %f } }\n" +
                  "function class @A @A.f(class @A %this)\n" +
                  "function class @B @B.f(class @B %this)\n"
    programIsCorrect(program)
  }
}
