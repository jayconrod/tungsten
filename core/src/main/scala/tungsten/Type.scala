package tungsten

import Utilities._

abstract sealed class Type
  extends Copying[Type]
{
  def validate(module: Module, location: Location): List[CompileException] = Nil
  def size(module: Module): Long
  def isNumeric: Boolean
  def isPointer: Boolean = false
  def isSubtypeOf(ty: Type): Boolean = ty == this
  final def <<:(ty: Type): Boolean = ty isSubtypeOf this
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
}

final case object NullType
  extends Type
{
  def size(module: Module) = wordSize(module)

  def isNumeric = false

  override def isPointer = true

  override def isSubtypeOf(ty: Type) = ty == NullType || ty.isInstanceOf[PointerType]
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
}

final case class StructType(structName: Symbol)
  extends Type
{
  override def validate(module: Module, location: Location) = {
    module.validateName[Struct](structName, location)
  }

  def size(module: Module): Long = {
    val struct = module.getStruct(structName)
    struct.size(module)
  }

  def isNumeric = false
}

final case class FunctionType(returnType: Type,
                              parameterTypes: List[Type])
  extends Type
{
  def size(module: Module) = throw new UnsupportedOperationException

  def isNumeric = false
}

final case class ClassType(className: Symbol,
                           typeArguments: List[Type] = Nil)
  extends Type
{
  override def validate(module: Module, location: Location): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }

  override def size(module: Module) = wordSize(module)

  override def isNumeric = false
}

final case class InterfaceType(interfaceName: Symbol,
                               typeArguments: List[Type] = Nil)
  extends Type
{
  override def validate(module: Module, location: Location): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }

  override def size(module: Module) = wordSize(module)

  override def isNumeric = false
}

final case class VariableType(variableName: Symbol)
  extends Type
{
  override def validate(module: Module, location: Location): List[CompileException] = {
    // TODO
    throw new UnsupportedOperationException
  }

  override def size(module: Module) = wordSize(module)

  override def isNumeric = false
}

