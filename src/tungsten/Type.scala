package tungsten

import Utilities._

abstract sealed class Type(location: Location) 
  extends TungstenObject(location)
{
  def validate(module: Module): List[CompileException] = Nil
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
  def equals(that: Any): Boolean
  def hashCode: Int
  def toString: String
}

final case class UnitType(override location: Location = Nowhere) extends Type(location) {
  def defaultValue(module: Module) = UnitValue(location)

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case UnitType(_) => true
      case _ => false
    }
  }

  override def hashCode = hash(0, "unit")

  override def toString = "#unit"
}

final case class BooleanType(override location: Location = Nowhere) extends Type(location) {
  def defaultValue(module: Module) = BooleanValue(false, location)

  def isNumeric = false

  override def supportsOperator(op: BinaryOperator) = {
    import BinaryOperator._
    List(AND, XOR, OR).contains(op)
  }

  override def equals(that: Any) = {
    that match {
      case BooleanType(_) => true
      case _ => false
    }
  }

  override def hashCode = hash(0, "boolean")

  override def toString = "#boolean"
}

final case class IntType(width: Int, 
                         override location: Location = Nowhere) 
  extends Type(location)
{
  if (width < 1 || !isPowerOf2(width) || width > 64)
    throw new IllegalArgumentException

  def defaultValue(module: Module) = {
    width match {
      case 8 => Int8Value(0, location)
      case 16 => Int16Value(0, location)
      case 32 => Int32Value(0, location)
      case 64 => Int64Value(0, location)
    }
  }

  def isNumeric = true

  override def supportsOperator(op: BinaryOperator) = true

  override def supportsOperator(op: RelationalOperator) = true

  override def equals(that: Any) = {
    that match {
      case IntType(w, _) if width == w => true
      case _ => false
    }
  }

  override def hashCode = List[Any]("int", width).foldLeft(0)(Utilities.hash _)

  override def toString = "#int" + width
}

object IntType {
  def wordType(module: Module) = {
    val wordSize = if (module.is64Bit) 64 else 32
    IntType(wordSize)
  }
}

final case class FloatType(width: Int, 
                           override location: Location = Nowhere)
  extends Type(location)
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException

  def defaultValue(module: Module) = {
    width match {
      case 32 => Float32Value(0.0f, location)
      case 64 => Float64Value(0.0, location)
    }
  }

  def isNumeric = true

  override def supportsOperator(op: BinaryOperator) = {
    import BinaryOperator._
    List(MULTIPLY, DIVIDE, REMAINDER, ADD, SUBTRACT).contains(op)
  }

  override def supportsOperator(op: RelationalOperator) = true

  override def equals(that: Any) = {
    that match {
      case FloatType(w, _) if width == w => true
      case _ => false
    }
  }

  override def hashCode = List[Any]("float", width).foldLeft(0)(hash _)

  override def toString = "#float" + width
}

final case class PointerType(elementType: Type,
                             override location: Location = Nowhere)
  extends Type(location)
{
  override def validate(module: Module) = elementType.validate(module)

  def defaultValue(module: Module) = NullValue()

  def isNumeric = false

  override def isPointer = true

  override def equals(that: Any) = {
    that match {
      case PointerType(et, _) if elementType == et => true
      case _ => false
    }
  }

  override def hashCode = List[Any]("pointer", elementType).foldLeft(0)(hash _)

  override def toString = elementType + "*"
}

final case class NullType(override location: Location = Nowhere)
  extends Type(location)
{
  def defaultValue(module: Module) = NullValue()

  def isNumeric = false

  override def isPointer = true

  override def isSubtypeOf(ty: Type) = ty.isInstanceOf[NullType] || ty.isInstanceOf[PointerType]

  override def equals(that: Any) = that.isInstanceOf[NullType]

  override def hashCode = hash(0, "null")

  override def toString = "#null"
}

final case class ArrayType(size: Option[Int],
                           elementType: Type,
                           override location: Location = Nowhere)
  extends Type(location)
{
  size match {
    case Some(s) if s < 0 => throw new IllegalArgumentException
    case _ => ()
  }

  override def validate(module: Module) = elementType.validate(module)

  def defaultValue(module: Module) = {
    val defaultElementValue = elementType.defaultValue(module)
    val defaultSize = size.getOrElse(0)
    ArrayValue(elementType, List.fill(defaultSize)(defaultElementValue), location)
  }

  def isNumeric = false

  override def isSubtypeOf(ty: Type) = {
    ty match {
      case ArrayType(otherSize, otherElementType, _) if elementType == otherElementType => {
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

  override def equals(that: Any) = {
    that match {
      case ArrayType(s, et, _) if size == s && elementType == et => true
      case _ => false
    }
  }

  override def hashCode = hash("array", size, elementType)

  override def toString = {
    val sizeStr = size.getOrElse("?")
    "[" + sizeStr + " * " + elementType + "]"
  }
}

final case class StructType(structName: Symbol,
                            override location: Location = Nowhere)
  extends Type(location)
{
  override def validate(module: Module) = module.validateName[Struct](structName, location)

  def defaultValue(module: Module) = {
    val struct = module.getStruct(structName)
    val fields = module.getFields(struct.fields)
    val elements = fields.map(_.ty.defaultValue(module))
    StructValue(structName, elements, location)
  }

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case StructType(n, _) if structName == n => true
      case _ => false
    }
  }

  override def hashCode = hash("StructType", structName)

  override def toString = structName.toString
}

final case class FunctionType(returnType: Type,
                              parameterTypes: List[Type], 
                              override location: Location = Nowhere)
  extends Type(location)
{
  def defaultValue(module: Module) = throw new UnsupportedOperationException

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case FunctionType(rt, pts, _) 
      if returnType == rt && parameterTypes.sameElements(pts) => true
      case _ => false
    }
  }

  override def hashCode = {
    val parts = "function" :: returnType :: parameterTypes.asInstanceOf[List[Any]]
    parts.foldLeft(0)(hash _)
  }

  override def toString = returnType + parameterTypes.mkString("(", ", ", ")")
}

