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

class InstructionValidationTest
  extends ValidationTest
{
  @Test
  def binopNonNumeric {
    val code = "unit %a = binop () + ()"
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def binopMismatch {
    val code = "int32 %a = binop int32 12 + int8 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def relopMismatch {
    val code = "boolean %a = relop int32 12 == ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def relopNonNumeric {
    val code = "boolean %a = relop () < ()"
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def nonExistantBranchCondition {
    val code = "cond true ? %foo() : %bar()"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def nonBooleanCondition {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond int32 12 ? @main.foo( ) : @main.bar( )\n" +
                  "  }\n" +
                  "  block %foo { return () }\n" +
                  "  block %bar { return () }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def conditionArgumentCount {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond true ? @main.foo(int32 12) : @main.bar(int32 12)\n" +
                  "  }\n" +
                  "  block %foo(int32 %x) { return () }\n" +
                  "  block %bar { return () }\n" +
                  "}\n"
    programContainsError[FunctionArgumentCountException](program)
  }

  @Test
  def conditionTypeMismatch {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    cond true ? @main.foo(int32 12) : @main.bar(int32 12)\n" +
                  "  }\n" +
                  "  block %foo(int32 %x) { return () }\n" +
                  "  block %bar(boolean %x) { return () }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def staticCallMissingFunction {
    val code = "unit %c = scall @foo( )"
    codeContainsError[UndefinedSymbolException](code)
  }

  @Test
  def staticCallTypeParameterCount {
    val program = "function unit @f[type %T]\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    scall @f()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeArgumentCountException](program)
  }

  @Test
  def staticCallTypeParameterBounds {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "function unit @f[type %T <: class @A]\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    scall @f[class @R]()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeArgumentBoundsException](program)
  }

  @Test
  def staticCallTypeArgumentApplication {
    val program = "class @R { constructors { %ctor } }\n" +
                  "function unit @R.ctor(class @R %this)\n" +
                  "function type %T @id[type %T](type %T %x)\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    class @R %x = new @R.ctor()\n" +
                  "    class @R %y = scall @id[class @R](class @R %x)\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def floatBitOp {
    val code = "float64 %a = binop float64 1. & float64 2."
    codeContainsError[UnsupportedNumericOperationException](code)
  }

  @Test
  def upcastToNonPointer {
    val code = "int32 %a = upcast null"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastFromNonPointer {
    val code = "int32* %a = upcast int32 12"
    codeContainsError[UpcastException](code)
  }

  @Test
  def upcastDown {
    val code = "int32* %a = upcast null" + 
               "nulltype %b = upcast int32* %a"
    codeContainsError[UpcastException](code)
  }

  @Test
  def stackAllocateInt {
    val code = "int32 %a = stack"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def stackAllocateNull {
    val code = "nulltype %a = stack"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateInt {
    val code = "int32 %a = heap"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def heapAllocateNull {
    val code = "nulltype %a = heap"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadInt {
    val code = "int32 %a = load int32 12"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def loadNull {
    val code = "unit %a = load null"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeInt {
    val code = "store int32 12, int32 34"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def storeNull {
    val code = "store int32 34, null"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def extractArrayOutOfBounds {
    val code = "int32 %a = extract [2 x int32] {int32 12, int32 34}, int64 5"
    codeIsCorrect(code)
  }

  @Test
  def extractBadIndexType {
    val code = "int32 %a = extract [2 x int32] {int32 12, int32 34}, int32 5"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def extractTooManyIndices {
    val code = "int32 %a = extract [2 x int32] {int32 12, int32 34}, int64 5, int64 6"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def insertType {
    val code = "insert (), [2 x int32] {int32 12, int32 64}, int64 0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def pointerCall {
    val code = "pcall int64 0()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def pointerCallCorrect {
    val code = "pcall ->unit @main()"
    codeIsCorrect(code)
  }

  @Test
  def stackArrayAllocCountType {
    val code = "unit* %a = stackarray ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressNonPointer {
    val code = "unit* %a = address (), int64 0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressBadIndexType {
    val code = "unit* %a = stackarray int64 5\n" +
               "unit* %b = address unit* %a, int32 1"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def addressTooManyIndices {
    val code = "unit* %a = stackarray int64 5\n" +
               "unit* %b = address unit* %a, int64 1, int64 1"
    codeContainsError[InvalidIndexException](code)
  }

  @Test
  def addressTwice {
    val code = "[2 x int32]* %a = stackarray int64 2\n" +
               "int32* %b = address [2 x int32]* %a, int64 1, int64 1\n" +
               "store int32 12, int32* %b"
    codeIsCorrect(code)
  }

  @Test
  def globalAddress {
    val program = "is64bit: true\n" +
                  "global [2 x int32] @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    int32* %a = address [2 x int32]* @foo, int64 0, int64 1\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programIsCorrect(program)
  }

  @Test
  def invalidStructValueCount {
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    unit %x = extract struct @A {(), ()}, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[FieldCountException](program)
  }

  @Test
  def structValueFieldType {
    val program = "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    unit %x = extract struct @A {int32 12}, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def extractFromStruct {
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    unit %c = extract struct @A {()}, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def extractNonLiteralIndex {
    val program = "struct @A {\n" +
                  "  field unit %b\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    int64 %i = binop int64 0 + int64 0\n" +
                  "    unit %e = extract struct @A {()}, int64 %i\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidIndexException](program)
  }

  @Test
  def insertDoubleIndex {
    val program = "is64bit: true\n" +
                  "struct @A {\n" +
                  "  field int32 %x\n" +
                  "}\n" +
                  "struct @B {\n" +
                  "  field struct @A %y\n" +
                  "}\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    struct @B %a = insert int32 34, struct @B { struct @A { int32 12 } }, int64 0, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def loadElementDoubleIndex {
    val code = "[1 x [1 x unit]]* %a = stack\n" +
               "unit %b = loadelement [1 x [1 x unit]]* %a, int64 0, int64 0, int64 0"
    codeIsCorrect(code)
  }

  @Test
  def storeElementDoubleIndex {
    val code = "[1 x [1 x unit]]* %a = stack\n" +
               "storeelement (), [1 x [1 x unit]]* %a, int64 0, int64 0, int64 0"
    codeIsCorrect(code)
  }

  @Test
  def nonExistantStructValue {
    val i1 = ExtractInstruction("i1", UnitType, StructValue("A", List(UnitValue)), List(IntValue(0, 64)))
    val i2 = ReturnInstruction("i2", UnitType, UnitValue)
    val block = Block("main.entry", Nil, List(i1, i2).map(_.name))
    val function = Function("main", UnitType, Nil, Nil, List(block.name))
    val module = (new Module).add(i1, i2, block, function)
    val errors = module.validate
    containsError[UndefinedSymbolException](errors)
  }

  @Test
  def integerTruncate {
    codeContainsError[TypeMismatchException]("int8 %a = itruncate ()")
    codeContainsError[TypeMismatchException]("unit %a = itruncate int64 12")
    codeContainsError[NumericTruncationException]("int64 %a = itruncate int8 12")
  }

  @Test
  def integerExtend {
    codeContainsError[NumericExtensionException]("int8 %a = isextend int64 12")
    codeContainsError[NumericExtensionException]("int8 %a = izextend int64 12")
  }

  @Test
  def floatTruncate {
    codeContainsError[TypeMismatchException]("float32 %a = ftruncate ()")
    codeContainsError[TypeMismatchException]("unit %a = ftruncate float64 3.2")
    codeContainsError[NumericTruncationException]("float64 %a = ftruncate float32 3.2")
  }

  @Test
  def floatExtend {
    codeContainsError[NumericExtensionException]("float32 %a = fextend float64 3.2")
  }

  @Test
  def floatIntCast {
    codeContainsError[TypeMismatchException]("float32 %a = itof ()")
    codeContainsError[TypeMismatchException]("unit %a = itof int32 12")
    codeContainsError[TypeMismatchException]("int32 %a = itof ()")
    codeContainsError[TypeMismatchException]("unit %a = itof float64 3.4")
  }

  @Test
  def indexTypeIn32Bit {
    val program = "is64bit: false\n" +
                  "function unit @main {\n" +
                  "  block %entry( ) {\n" +
                  "    [3 x unit]* %a = stack\n" +
                  "    unit* %b = address [3 x unit]* %a, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def bitcastCorrect {
    val code = "int64 %a = bitcast int64 0"
    codeIsCorrect(code)
  }

  @Test
  def bitcastSizes {
    val code = "int64 %a = bitcast int32 0"
    codeContainsError[InvalidBitCastException](code)
  }

  @Test
  def newInvalidType {
    val program = "class @R\n" +
                  "class @A <: class @R\n" +
                  "class @C[type %T <: class @A] <: class @R {\n" +
                  "  constructors { %ctor }\n" +
                  "}\n" +
                  "function unit @C.ctor[type %T](class @C[type %T] %this)\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @C[class @R] %x = new @C.ctor()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeArgumentBoundsException](program)
  }

  @Test
  def newWrongTypeArguments {
    val program = "class @R { constructors { %ctor } }\n" +
                  "function unit @R.ctor[type %T](class @R %this)\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @R %x = new @R.ctor()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeArgumentCountException](program)
  }

  @Test
  def newNonConstructor {
    val program = "class @R\n" +
                  "function unit @R.ctor(class @R %this)\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @R %x = new @R.ctor()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[NewConstructorException](program)
  }

  @Test
  def newAbstractClass {
    val program = "annotation @tungsten.Abstract\n" +
                  "class @R\n" +
                  "@tungsten.Abstract class @A <: class @R { constructors { %ctor } }\n" +
                  "function unit @A.ctor(class @A %this)\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @A %x = new @A.ctor()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[NewAbstractException](program)
  }

  @Test
  def newNonReified {
    val program = "class @R\n" +
                  "class @A[type %T] <: class @R {\n" +
                  "  constructors { %ctor }\n" +
                  "}\n" +
                  "function unit @A.ctor[type %T](class @A[type %T] %this)\n" +
                  "function unit @f[type %T] {\n" +
                  "  block %entry {\n" +
                  "    class @A[type @f.T] %x = new @A.ctor()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[NonReifiedTypeParameterException](program)
  }

  @Test
  def virtualCallNonClass {
    val code = "vcall int64 0:0()\n"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def virtualCallNothing {
    val code = "nothing %a = bitcast null\n" +
               "vcall nothing %a:0()\n"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def virtualCallInvalidIndex {
    val program = "class @R {\n" +
                  "  constructors { %ctor }\n" +
                  "}\n" +
                  "function unit @R.ctor(class @R %this)\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    class @R %x = new @R.ctor()\n" +
                  "    vcall class @R %x:0()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidVirtualMethodIndexException](program)
  }

  @Test
  def virtualCallVariableType {
    val program = "class @R {\n" +
                  "  constructors { %ctor }\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "function unit @R.ctor(class @R %this)\n" +
                  "function unit @R.f(class @R %this)\n" +
                  "function unit @f[type %T](type %T %x) {\n" +
                  "  block %entry {\n" +
                  "    vcall type @f.T @f.x:0()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def virtualCallWithTypeParameter {
    val program = "class @R { constructors { %ctor } }\n" +
                  "class @C[type %T] <: class @R {\n" +
                  "  constructors { %ctor }\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "function unit @R.ctor(class @R %this)\n" +
                  "function unit @C.ctor[type %T](class @C[type %T] %this)\n" +
                  "function unit @C.f[type %T, type %S](class @C[type %T] %this, type %S %x)\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @R %a = new @R.ctor()\n" +
                  "    class @C[class @R] %b = new @C.ctor()\n" +
                  "    vcall class @C[class @R] %b:0[class @R](class @R %a)\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def bitCastValueSize {
    val program = "global int64 @g = bitcast int32 2 to int64"
    programContainsError[InvalidBitCastException](program)
  }

  @Test
  def loadVariableType {
    val program = "class @R {\n" +
                  "  field int64 %x\n" +
                  "}\n" +
                  "function unit @f[type %T <: class @R](type %T %x) {\n" +
                  "  block %entry {\n" +
                  "    int64 %x = loadelement type @f.T @f.x, int64 0, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def loadClassType {
    val program = "class @R {\n" +
                  "  field int64 %x\n" +
                  "}\n" +
                  "function unit @f(class @R %r) {\n" +
                  "  block %entry {\n" +
                  "    int64 %x = loadelement class @R @f.r, int64 0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def nullCastToClass {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class? @R %x = upcast null\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def instructionScope {
    val program = "function int64 @f {\n" +
                  "  block %entry {\n" +
                  "    return int64 @f.a.x\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    int64 %x = binop int64 1 + int64 2\n" +
                  "    return int64 %x\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[ScopeException](program)
  }

  @Test
  def instructionOrder {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    int64 %a = binop int64 1 + int64 %b\n" +
                  "    int64 %b = binop int64 2 + int64 3\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[ScopeException](program)
  }

  @Test
  def vlookupNotObject = {
    val code = "vlookup ():0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def vlookupNothing {
    val code = "nothing %a = bitcast null\n" +
               "vlookup nothing %a:0"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def vlookupInvalidIndex = {
    val program = "class @R {\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "function unit @R.f(class @R %this) {\n" +
                  "  block %entry {\n" +
                  "    (class @R)->unit %f = vlookup class @R @R.f.this:1\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidVirtualMethodIndexException](program)
  }

  @Test
  def vlookupTypeException = {
    val program = "class @R {\n" +
                  "  methods { %f }\n" +
                  "}\n" +
                  "function unit @R.f(class @R %this) {\n" +
                  "  block %entry {\n" +
                  "    unit %x = vlookup class @R @R.f.this:0\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def throwNotUnit {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @R %x = upcast null\n" +
                  "    int64 %y = throw class @R %x\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def throwExceptionNotObject {
    val code = "throw ()"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def throwNotTerminating {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @R %x = upcast null\n" +
                  "    throw class @R %x\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[EarlyTerminationException](program)
  }

  @Test
  def catchNonPointer {
    val code = "unit %x = catch"
    codeContainsError[TypeMismatchException](code)
  }

  @Test
  def instanceofNotBoolean {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    unit %x = instanceof bitcast null to class @R: class @R\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def instanceofNonClassTarget {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    boolean %x = instanceof (): class @R\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def instanceOfNonClassType {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    boolean %x = instanceof bitcast null to class @R: int64\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def instanceOfNullabilityMismatch {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    boolean %x = instanceof bitcast null to class? @R: class @R\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def instanceOfNonReified {
    val program = "class @R\n" +
                  "function unit @f[type %T] {\n" +
                  "  block %entry {\n" +
                  "    boolean %x = instanceof bitcast null to class @R: type @f.T\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[NonReifiedTypeParameterException](program)
  }

  @Test
  def checkedCastFromNonObject {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @R %x = checkedcast int64 12\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def checkedCastToNonObject {
    val program = "class @R\n" +
                  "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    int64 %x = checkedcast bitcast null to class @R\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def checkedCastToNonReified {
    val program = "class @R\n" +
                  "function unit @f[type %T] {\n" +
                  "  block %entry {\n" +
                  "    type @f.T %x = checkedcast bitcast null to class @R\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[NonReifiedTypeParameterException](program)
  }
}
