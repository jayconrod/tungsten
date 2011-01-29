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
  def isSubtypeOf(ty: Type, module: Module): Boolean = ty == this
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

final case class PointerType(elementType: Type)
  extends Type
{
  override def validate(module: Module, location: Location) = {
    elementType.validate(module, location)
  }

  def size(module: Module) = wordSize(module)

  def isNumeric = false

  override def isPointer = true

  override def expose(module: Module): PointerType = {
    PointerType(elementType.expose(module))
  }
}

final case object NullType
  extends Type
{
  def size(module: Module) = wordSize(module)

  def isNumeric = false

  override def isPointer = true

  override def isSubtypeOf(ty: Type, module: Module) = {
    ty == NullType || ty.isInstanceOf[PointerType]
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

  def size(module: Module) = throw new UnsupportedOperationException

  def isNumeric = false

  override def isSubtypeOf(ty: Type, module: Module): Boolean = {
    def isExposedSubtypeOf(s: Type, t: Type): Boolean = {
      val es = s.expose(module)
      val et = t.expose(module)
      es.isSubtypeOf(et, module)
    }

    ty match {
      case fty: FunctionType => {
        if (typeParameters.size != fty.typeParameters.size ||
            parameterTypes.size != fty.parameterTypes.size)
        {
          false
        } else {
          val typeParameterDefns = module.getTypeParameters(typeParameters)
          val otherTypeParameterDefns = module.getTypeParameters(fty.typeParameters)
          val typeParametersMatch = (typeParameterDefns zip otherTypeParameterDefns) forall { p =>
            val (typeParameter, otherTypeParameter) = p
            typeParameter.boundsMatch(otherTypeParameter)
          }

          val parametersAreSupertypes = (parameterTypes zip fty.parameterTypes) forall { p =>
            val (parameterType, otherParameterType) = p
            isExposedSubtypeOf(otherParameterType, parameterType)
          }

          val returnTypeIsSubtype = isExposedSubtypeOf(returnType, fty.returnType)

          typeParametersMatch && parametersAreSupertypes && returnTypeIsSubtype
        }
      }
      case _ => false
    }
  }

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
  extends Type
{
  override def size(module: Module) = wordSize(module)

  override def isNumeric = false

  override def isPointer = true

  override def isObject = true

  def definitionName: Symbol

  def typeArguments: List[Type]

  def typeParameters(module: Module): List[TypeParameter] = {
    getDefinition(module).getTypeParameters(module)
  }

  def supertype(module: Module): Option[ObjectType] = {
    getDefinition(module).getSuperType
  }

  def getDefinition(module: Module): ObjectDefinition

  override def isSubtypeOf(ty: Type, module: Module): Boolean = {
    ty match {
      case VariableType(variableName) => {
        val tyVar = module.getTypeParameter(variableName)
        tyVar.lowerBound match {
          case Some(bound) => isSubtypeOf(bound, module)
          case None => false
        }
      }
      case objTy: ObjectType => {
        val defn = module.getObjectDefinition(definitionName)
        if (definitionName == objTy.definitionName) {
          val tyParams = typeParameters(module)
          val selfTyArgs = typeArguments
          val otherTyArgs = objTy.typeArguments
          (tyParams zip (selfTyArgs zip otherTyArgs)) forall { pa =>
            val (tyParam, (selfTyArg, otherTyArg)) = pa
            tyParam.variance match {
              case Variance.COVARIANT => selfTyArg.isSubtypeOf(otherTyArg, module)
              case Variance.CONTRAVARIANT => otherTyArg.isSubtypeOf(selfTyArg, module)
              case Variance.INVARIANT => selfTyArg == otherTyArg
            }
          }
        } else {
          val inheritedTypes = defn.substitutedInheritedTypes(typeArguments)
          inheritedTypes.exists { _.isSubtypeOf(objTy, module) }
        }
      }
      case _ => false
    }
  }

  def validateTypeArgumentCount(module: Module, 
                                location: Location): List[CompileException] = 
  {
    if (typeArguments.size != typeParameters(module).size) {
      List(TypeArgumentCountException(definitionName, 
                                      typeArguments.size, 
                                      typeParameters(module).size,
                                      location))
    } else
      Nil
  }

  def validateTypeArguments(module: Module, location: Location): List[CompileException] = {
    (typeArguments zip typeParameters(module)) flatMap { ap =>
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
    val defn = module.getObjectDefinition(definitionName)
    val typeParameters = module.getTypeParameters(defn.typeParameters)
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
                           typeArguments: List[Type] = Nil)
  extends Type
  with ObjectType
{
  override def validateComponents(module: Module, location: Location): List[CompileException] = {
    stage(module.validateName[Class](className, location),
          validateTypeArgumentCount(module, location))
  }

  override def validate(module: Module, location: Location): List[CompileException] = {
    validateTypeArguments(module, location)
  }

  override def isRootClassType(module: Module): Boolean = {
    val clas = module.getClass(className)
    !clas.superclass.isDefined
  }

  def definitionName = className

  override def expose(module: Module): ClassType = {
    ClassType(className, typeArguments.map(_.expose(module)))
  }

  def getDefinition(module: Module): Class = {
    module(className).asInstanceOf[Class]
  }
}

final case class InterfaceType(interfaceName: Symbol,
                               typeArguments: List[Type] = Nil)
  extends Type
  with ObjectType
{
  override def validateComponents(module: Module, location: Location): List[CompileException] = {
    stage(module.validateName[Interface](interfaceName, location),
          validateTypeArgumentCount(module, location))
  }

  override def validate(module: Module, location: Location): List[CompileException] = {
    validateTypeArguments(module, location)
  }

  def definitionName = interfaceName

  override def expose(module: Module): InterfaceType = {
    InterfaceType(interfaceName, typeArguments.map(_.expose(module)))
  }

  def getDefinition(module: Module): Interface = {
    module(interfaceName).asInstanceOf[Interface]
  }
}

final case class VariableType(variableName: Symbol)
  extends Type
{
  override def validateComponents(module: Module, location: Location): List[CompileException] = {
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

  override def size(module: Module) = wordSize(module)

  override def isNumeric = false

  override def isPointer = true

  override def isObject = true

  override def isSubtypeOf(ty: Type, module: Module): Boolean = {
    val tyParam = module.getTypeParameter(variableName)
    tyParam.upperBound match {
      case Some(upper) => upper.isSubtypeOf(ty, module)
      case None => ty.isRootClassType(module)
    }
  }

  override def expose(module: Module): ObjectType = {
    val typeParameter = module.getTypeParameter(variableName)
    typeParameter.getUpperBoundType(module)
  }
}
