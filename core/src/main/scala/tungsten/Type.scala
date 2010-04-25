package tungsten

import Utilities._

abstract sealed class Type {
  def validate(module: Module, location: Location): List[CompileException] = Nil
  def defaultValue(module: Module): Value
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
  def defaultValue(module: Module) = UnitValue

  def isNumeric = false

  override def toString = "#unit"
}

final case object BooleanType extends Type {
  def defaultValue(module: Module) = BooleanValue(false)

  def isNumeric = false

  override def supportsOperator(op: BinaryOperator) = {
    import BinaryOperator._
    List(AND, XOR, OR).contains(op)
  }

  override def toString = "#boolean"
}

final case object CharType 
  extends Type
{
  def defaultValue(module: Module) = CharValue(0.toChar)

  def isNumeric = false

  override def supportsOperator(op: RelationalOperator) = true

  override def toString = "#char"
}

final case object StringType
  extends Type
{
  def defaultValue(module: Module) = StringValue("")

  def isNumeric = false

  override def supportsOperator(op: RelationalOperator) = true

  override def toString = "#string"
}

final case class IntType(width: Int)
  extends Type
{
  if (width < 8 || !isPowerOf2(width) || width > 64)
    throw new IllegalArgumentException

  def defaultValue(module: Module) = {
    width match {
      case 8 => IntValue(0, 8)
      case 16 => IntValue(0, 16)
      case 32 => IntValue(0, 32)
      case 64 => IntValue(0, 64)
    }
  }

  def isNumeric = true

  override def supportsOperator(op: BinaryOperator) = true

  override def supportsOperator(op: RelationalOperator) = true

  override def toString = "#int" + width
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

  def defaultValue(module: Module) = {
    width match {
      case 32 => FloatValue(0.0, 32)
      case 64 => FloatValue(0.0, 64)
    }
  }

  def isNumeric = true

  override def supportsOperator(op: BinaryOperator) = {
    import BinaryOperator._
    List(MULTIPLY, DIVIDE, REMAINDER, ADD, SUBTRACT).contains(op)
  }

  override def supportsOperator(op: RelationalOperator) = true

  override def toString = "#float" + width
}

final case class PointerType(elementType: Type)
  extends Type
{
  override def validate(module: Module, location: Location) = {
    elementType.validate(module, location)
  }

  def defaultValue(module: Module) = NullValue

  def isNumeric = false

  override def isPointer = true

  override def toString = elementType + "*"
}

final case object NullType
  extends Type
{
  def defaultValue(module: Module) = NullValue

  def isNumeric = false

  override def isPointer = true

  override def isSubtypeOf(ty: Type) = ty == NullType || ty.isInstanceOf[PointerType]

  override def toString = "#null"
}

final case class ArrayType(size: Option[Int], elementType: Type)
  extends Type
{
  size match {
    case Some(s) if s < 0 => throw new IllegalArgumentException
    case _ => ()
  }

  override def validate(module: Module, location: Location) = {
    elementType.validate(module, location)
  }

  def defaultValue(module: Module) = {
    val defaultElementValue = elementType.defaultValue(module)
    val defaultSize = size.getOrElse(0)
    ArrayValue(elementType, List.fill(defaultSize)(defaultElementValue))
  }

  def isNumeric = false

  override def isSubtypeOf(ty: Type) = {
    ty match {
      case ArrayType(otherSize, otherElementType) if elementType == otherElementType => {
        (size, otherSize) match {
          case (Some(n), Some(m)) => n == m
          case (Some(_), None) => true
          case (None, Some(_)) => false
          case (None, None) => true
        }
      }
      case _ => false
    }
  }

  override def toString = {
    val sizeStr = size.map(_.toString).getOrElse("?")
    "[" + sizeStr + " * " + elementType + "]"
  }
}

final case class StructType(structName: Symbol)
  extends Type
{
  override def validate(module: Module, location: Location) = {
    module.validateName[Struct](structName, location)
  }

  def defaultValue(module: Module) = {
    val struct = module.getStruct(structName)
    val fields = module.getFields(struct.fields)
    val elements = fields.map(_.ty.defaultValue(module))
    StructValue(structName, elements)
  }

  def isNumeric = false

  override def toString = structName.toString
}

final case class FunctionType(returnType: Type,
                              parameterTypes: List[Type])
  extends Type
{
  def defaultValue(module: Module) = throw new UnsupportedOperationException

  def isNumeric = false
}

