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

abstract sealed class Type
  extends Copying[Type]
{
  def validateComponents(module: Module, location: Location): List[CompileException] = Nil
  def validate(module: Module, location: Location): List[CompileException] = Nil
  def validateVariance(positionVariance: Variance,
                       module: Module,
                       location: Location): List[CompileException] = Nil
  def size(module: Module): Long
  def isNumeric: Boolean
  def isPointer: Boolean = false
  def isObject: Boolean = false
  final def isSubtypeOf(ty: Type, module: Module): Boolean = {
    TypeUtilities.subtype(this, ty, module)
  }
  def isRootClassType(module: Module): Boolean = false
  def substitute(fromName: Symbol, toType: Type): Type = {
    val fromType = VariableType(fromName)
    mapTypes { ty => if (ty == fromType) toType else ty }
  }
  def substitute(fromNames: List[Symbol], toTypes: List[Type]): Type = {
    if (fromNames.size != toTypes.size)
      throw new IllegalArgumentException
    else {
      (this /: (fromNames zip toTypes)) { (ty, p) =>
        val (fromName, toType) = p
        ty.substitute(fromName, toType)
      }
    }
  }
  def expose(module: Module): Type = this
  def supportsOperator(op: BinaryOperator) = false
  def supportsOperator(op: RelationalOperator) = {
    import RelationalOperator._
    List(EQUAL, NOT_EQUAL).contains(op)
  }
}

sealed trait ReferenceType 
  extends Type
{
  override def isPointer = true

  def pointerFlags: Int
  def withPointerFlags(flags: Int) : ReferenceType
  def setPointerFlags(flags: Int) = withPointerFlags(pointerFlags | flags)
  def clearPointerFlags(flags: Int) = withPointerFlags(pointerFlags & ~flags)

  def isNullable = (pointerFlags & ReferenceType.NULLABLE) != 0

  override def validateComponents(module: Module, location: Location): List[CompileException] = {
    if ((pointerFlags & ~ReferenceType.ALL_FLAGS) != 0)
      List(InvalidPointerFlagsException(this, location))
    else
      Nil
  }
}

object ReferenceType {
  val NULLABLE = 1
  val ALL_FLAGS = NULLABLE
}

final case object UnitType 
  extends Type 
{
  def size(module: Module) = 0L

  def isNumeric = false
}

final case object BooleanType extends Type {
  def size(module: Module) = 1L

  def isNumeric = false

  override def supportsOperator(op: BinaryOperator) = {
    import BinaryOperator._
    List(AND, XOR, OR).contains(op)
  }
}

final case object CharType 
  extends Type
{
  def size(module: Module) = 2L

  def isNumeric = false

  override def supportsOperator(op: RelationalOperator) = true
}

final case object StringType
  extends Type
{
  def size(module: Module) = wordSize(module)

  def isNumeric = false

  override def supportsOperator(op: RelationalOperator) = true
}

final case class IntType(width: Int)
  extends Type
{
  if (width < 8 || !isPowerOf2(width) || width > 64)
    throw new IllegalArgumentException

  def maxValue: Long = (1L << width - 1) - 1L

  def minValue: Long = -1L << width - 1

  def size(module: Module) = width / 8

  def isNumeric = true

  override def supportsOperator(op: BinaryOperator) = true

  override def supportsOperator(op: RelationalOperator) = true
}

object IntType {
  def wordSize(module: Module) = if (module.is64Bit) 64 else 32

  def wordType(module: Module) = IntType(wordSize(module))
}

final case class FloatType(width: Int)
  extends Type
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException

  def size(module: Module) = width / 8

  def isNumeric = true

  override def supportsOperator(op: BinaryOperator) = {
    import BinaryOperator._
    List(MULTIPLY, DIVIDE, REMAINDER, ADD, SUBTRACT).contains(op)
  }

  override def supportsOperator(op: RelationalOperator) = true
}

final case class PointerType(elementType: Type, pointerFlags: Int = 0)
  extends ReferenceType
{
  def withPointerFlags(pointerFlags: Int) = copy(pointerFlags = pointerFlags)

  override def validate(module: Module, location: Location) = {
    elementType.validate(module, location)
  }

  def size(module: Module) = wordSize(module)

  def isNumeric = false

  override def expose(module: Module): PointerType = {
    PointerType(elementType.expose(module))
  }
}

final case object NullType
  extends ReferenceType
{
  def size(module: Module) = wordSize(module)

  def isNumeric = false

  def pointerFlags = ReferenceType.NULLABLE

  def withPointerFlags(flags: Int) = {
    if (flags != pointerFlags)
      throw new IllegalArgumentException("NullType can't be made non-nullable")
    else
      this
  }
}

final case class ArrayType(length: Long, elementType: Type)
  extends Type
{
  if (length < 0)
    throw new IllegalArgumentException

  override def validate(module: Module, location: Location) = {
    def validateLength = {
      if (!module.is64Bit && length > Integer.MAX_VALUE)
        List(ArrayTypeWidthException(length, location))
      else
        Nil
    }

    stage(validateLength,    
          elementType.validate(module, location))
  }

  def size(module: Module) = length * elementType.size(module)

  def isNumeric = false

  override def expose(module: Module): ArrayType = {
    ArrayType(length, elementType.expose(module))
  }
}

final case class StructType(structName: Symbol)
  extends Type
{
  override def validateComponents(module: Module, location: Location) = {
    module.validateName[Struct](structName, location)
  }

  def size(module: Module): Long = {
    val struct = module.getStruct(structName)
    struct.size(module)
  }

  def isNumeric = false
}

final case object VariadicType
  extends Type
{
  def size(module: Module) = wordSize(module)
  def isNumeric = false
}

final case class FunctionType(returnType: Type,
                              typeParameters: List[Symbol],
                              parameterTypes: List[Type])
  extends Type
{
  override def validateVariance(positionVariance: Variance,
                                module: Module,
                                location: Location) =
  {
    import Variance._
    val returnPositionVariance = positionVariance
    val returnErrors = returnType.validateVariance(returnPositionVariance, module, location)
    val parameterPositionVariance = positionVariance.opposite
    val parameterErrors = parameterTypes.flatMap(_.validateVariance(parameterPositionVariance, module, location))
    returnErrors ++ parameterErrors
  }

  override def validate(module: Module, location: Location): List[CompileException] = {
    val variadicIndex = parameterTypes.indexOf(VariadicType)
    if (variadicIndex != -1 && variadicIndex < parameterTypes.size - 1)
      List(VariadicTypeException(location))
    else
      Nil
  }

  def size(module: Module) = wordSize(module)

  def isNumeric = false

  def isVariadic = !parameterTypes.isEmpty && parameterTypes.last == VariadicType

  override def expose(module: Module): FunctionType = {
    val typeParameterBounds = module.getTypeParameters(typeParameters).map(_.getUpperBoundType(module))
    val substitutedReturnType = returnType.substitute(typeParameters, typeParameterBounds)
    val substitutedParameterTypes = parameterTypes.map(_.substitute(typeParameters, typeParameterBounds))
    FunctionType(substitutedReturnType, Nil, substitutedParameterTypes)
  }

  def applyTypeArguments(typeArguments: List[Type]): FunctionType = {
    assert(typeArguments.size == typeParameters.size)
    val substituted = substitute(typeParameters, typeArguments).asInstanceOf[FunctionType]
    substituted.copy(typeParameters = Nil)
  }
}

sealed trait ObjectType 
  extends ReferenceType
{
  override def size(module: Module) = wordSize(module)

  override def isNumeric = false

  override def isObject = true

  override def expose(module: Module): ObjectType = this
}

sealed trait ObjectDefinitionType
  extends ObjectType
{
  def definitionName: Symbol

  def typeArguments: List[ObjectType]

  /** Returns a list of object definition types inherited by this type (including
   *  this type). This works by traversing the inheritance graph and replacing type
   *  parameters with arguments, so it is not a fast method. Types are returned in
   *  depth first order. Supertypes are visited class first, then interfaces.
   */
  def supertypes(module: Module): List[ObjectDefinitionType] = {
    def visit(ty: ObjectDefinitionType,
              types: List[ObjectDefinitionType],
              visited: Set[Symbol]): (List[ObjectDefinitionType], Set[Symbol])  =
    {
      val name = ty.definitionName
      if (visited(name))
        (types, visited)
      else {
        val defn = ty.getObjectDefinition(module)
        val supertypes = defn.substitutedInheritedTypes(ty.typeArguments)
        ((ty :: types, visited + name) /: supertypes) { (ind, sty) =>
          val (types, visited) = ind
          visit(sty, types, visited)
        }
      }
    }
    visit(this, Nil, Set())._1.reverse
  }

  def objectDefinitionTypeParameters(module: Module): List[TypeParameter] = {
    val defn = getObjectDefinition(module)
    module.getTypeParameters(defn.typeParameters)
  }

  def getObjectDefinition(module: Module): ObjectDefinition = {
    module.getObjectDefinition(definitionName)
  }

  def validateTypeArgumentCount(module: Module, 
                                location: Location): List[CompileException] = 
  {
    if (typeArguments.size != objectDefinitionTypeParameters(module).size) {
      List(TypeArgumentCountException(definitionName, 
                                      typeArguments.size, 
                                      objectDefinitionTypeParameters(module).size,
                                      location))
    } else
      Nil
  }

  def validateTypeArguments(module: Module, location: Location): List[CompileException] = {
    (typeArguments zip objectDefinitionTypeParameters(module)) flatMap { ap =>
      val (argument, parameter) = ap
      if (!parameter.isArgumentInBounds(argument, module))
        List(TypeArgumentBoundsException(argument, parameter, location))
      else
        Nil
    }
  }

  override def validateVariance(positionVariance: Variance,
                                module: Module,
                                location: Location): List[CompileException] =
  {
    import Variance._
    val typeParameters = objectDefinitionTypeParameters(module)
    val parameterVariances = typeParameters.map(_.variance)
    (parameterVariances zip typeArguments) flatMap { case (parameterVariance, typeArgument) =>
      val parameterPositionVariance = if (parameterVariance == CONTRAVARIANT)
        positionVariance.opposite
      else
        positionVariance
      typeArgument.validateVariance(parameterPositionVariance, module, location)
    }
  }
}

final case class ClassType(className: Symbol,
                           typeArguments: List[ObjectType] = Nil,
                           pointerFlags: Int = 0)
  extends ObjectDefinitionType
{
  def withPointerFlags(pointerFlags: Int) = copy(pointerFlags = pointerFlags)

  override def validateComponents(module: Module, location: Location): List[CompileException] = {
    stage(super.validateComponents(module, location),
          module.validateName[Class](className, location),
          validateTypeArgumentCount(module, location))
  }

  override def validate(module: Module, location: Location): List[CompileException] = {
    validateTypeArguments(module, location)
  }

  override def isRootClassType(module: Module): Boolean = {
    val clas = module.getClass(className)
    !clas.superclass.isDefined
  }

  override def expose(module: Module): ClassType = {
    ClassType(className, typeArguments.map(_.expose(module)))
  }

  def definitionName = className
}

final case class InterfaceType(interfaceName: Symbol,
                               typeArguments: List[ObjectType] = Nil,
                               pointerFlags: Int = 0)
  extends ObjectDefinitionType
{
  def withPointerFlags(pointerFlags: Int) = copy(pointerFlags = pointerFlags)

  override def validateComponents(module: Module, location: Location): List[CompileException] = {
    stage(super.validateComponents(module, location),
          module.validateName[Interface](interfaceName, location),
          validateTypeArgumentCount(module, location))
  }

  override def validate(module: Module, location: Location): List[CompileException] = {
    validateTypeArguments(module, location)
  }

  override def expose(module: Module): InterfaceType = {
    InterfaceType(interfaceName, typeArguments.map(_.expose(module)))
  }

  def definitionName = interfaceName
}

final case class VariableType(variableName: Symbol, pointerFlags: Int = 0)
  extends ObjectType
{
  def withPointerFlags(pointerFlags: Int) = copy(pointerFlags = pointerFlags)

  override def validateComponents(module: Module, location: Location): List[CompileException] = {
    super.validateComponents(module, location) ++ 
      module.validateName[TypeParameter](variableName, location)
  }

  override def validateVariance(positionVariance: Variance,
                                module: Module,
                                location: Location): List[CompileException] =
  {
    import Variance._
    val parameterVariance = module.getTypeParameter(variableName).variance
    
    if (parameterVariance == INVARIANT ||
        parameterVariance == positionVariance)
      Nil
    else
      List(TypeParameterVarianceException(this, positionVariance, location))
  }

  def getUpperBoundType(module: Module): ObjectDefinitionType = {
    val typeParameter = module.getTypeParameter(variableName)
    typeParameter.getUpperBoundType(module)
  }

  def getLowerBoundType(module: Module): ObjectType = {
    val typeParameter = module.getTypeParameter(variableName)
    typeParameter.getLowerBoundType(module)
  }

  override def expose(module: Module): ObjectType = {
    getUpperBoundType(module)
  }
}

final case class NothingType(pointerFlags: Int = 0)
  extends ObjectType
{
  def withPointerFlags(pointerFlags: Int) = copy(pointerFlags = pointerFlags)
}

object TypeUtilities {
  /** The subtype relation for the Tungsten type system. If s is a subtype of t in module m,
   *  then values of s can be upcast and used as values of t with no runtime check.
   */
  def subtype(s: Type, t: Type, module: Module): Boolean = {
    (s, t) match {
      // Every type is a subtype of itself
      case _ if s == t => true

      // The null type is a subtype of all nullable reference types
      case (NullType, rt: ReferenceType) if rt.isNullable => true

      // A non-nullable pointer type is a subtype of a nullable pointer if type if their 
      // elements types are equal. Note that pointer types are never subtypes if they have
      // different element types, even if their element types are subtypes. Consider if this
      // were allowed:
      //   Assume S <: T
      //   S* %a = stack
      //   T* %b = upcast S* %a
      //   T %t = new ...
      //   store T %t, T* %b
      // Now %a points to a value of type T. 
      case (sp @ PointerType(se, snullable), tp @ PointerType(te, tnullable)) 
        if !sp.isNullable && tp.isNullable && se == te => true

      // A function type S is a subtype of another function type T if:
      //   - S's return type is a subtype of T's return type
      //   - S's parameter types are supertypes of T's parameter types
      //   - Type parameters of S and T have equal bounds
      case (FunctionType(rs, tps, ps), FunctionType(rt, tpt, pt)) => {
        if (tps.size != tpt.size || ps.size != pt.size)
          false
        else {
          val sTypeParameterDefns = module.getTypeParameters(tps)
          val tTypeParameterDefns = module.getTypeParameters(tpt)
          val typeParametersMatch = (sTypeParameterDefns zip tTypeParameterDefns) forall { i =>
            val (sTypeParameter, tTypeParameter) = i
            sTypeParameter boundsMatch tTypeParameter
          }

          val parametersAreSupertypes = (ps zip pt) forall { i =>
            val (sType, tType) = i
            exposedSubtype(tType, sType, module)
          }

          val returnTypeIsSubtype = exposedSubtype(rs, rt, module)

          typeParametersMatch && parametersAreSupertypes && returnTypeIsSubtype
        }
      }

      // Class or interface type S is a subtype of T if one of the following is true:
      //   - S and T are of the same class or interface and their type arguments are compatible
      //     according to the variance the type parameters are defined with
      //   - One of S's inherited types is a subtype of T
      case (ss: ObjectDefinitionType, tt: ObjectDefinitionType) 
        if !ss.isNullable && !tt.isNullable =>
      {
        val sDefn = ss.getObjectDefinition(module)
        if (sDefn.name == tt.definitionName) {
          val tyParams = sDefn.getTypeParameters(module)
          val sTyArgs = ss.typeArguments
          val tTyArgs = tt.typeArguments
          (tyParams zip (sTyArgs zip tTyArgs)) forall { i =>
            val (tyParam, (sTyArg, tTyArg)) = i
            tyParam.variance match {
              case Variance.COVARIANT => subtype(sTyArg, tTyArg, module)
              case Variance.CONTRAVARIANT => subtype(tTyArg, sTyArg, module)
              case Variance.INVARIANT => sTyArg == tTyArg
            }
          }
        } else {
          val inheritedTypes = sDefn.substitutedInheritedTypes(ss.typeArguments)
          inheritedTypes exists { it => subtype(it, tt, module) }
        }
      }

      // A type variable S is a subtype of an object type T if S's upper bound is a subtype
      // of T
      case (ss: VariableType, tt: ObjectType) 
        if !ss.isNullable && !tt.isNullable =>
      {
        subtype(ss.getUpperBoundType(module), tt, module)
      }

      // An object type S is a subtype of a type variable T if S is a subtype of T's lower bound
      case (ss: ObjectType, tt: VariableType)
        if !ss.isNullable && !tt.isNullable =>
      {
        subtype(ss, tt.getLowerBoundType(module), module)
      }

      // The Nothing type is a subtype of all object types
      case (ss: NothingType, tt: ObjectType)
        if !ss.isNullable && !tt.isNullable => true

      // A non-nullable type is a subtype of its nullable equivalent
      case (ss: ObjectType, tt: ObjectType) => {
        if (ss.isNullable && !tt.isNullable)
          false
        else {
          subtype(ss.clearPointerFlags(ReferenceType.NULLABLE), 
                  tt.clearPointerFlags(ReferenceType.NULLABLE), 
                  module)
        }
      }

      case _ => false
    }
  }

  def exposedSubtype(s: Type, t: Type, module: Module): Boolean = {
    subtype(s.expose(module), t.expose(module), module)
  }
}
