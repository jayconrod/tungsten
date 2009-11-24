package tungsten

import Utilities._

abstract sealed class Type(location: Location) 
  extends TungstenObject(location)
{
  def validate(module: Module): List[CompileException] = Nil
  def defaultValue: Value
  def isNumeric: Boolean
  def isPointer: Boolean = false
  def isSubtypeOf(ty: Type): Boolean = ty == this
  final def <<:(ty: Type): Boolean = ty isSubtypeOf this
  def equals(that: Any): Boolean
  def hashCode: Int
  def toString: String

  protected def validateName[T <: Definition](name: Symbol, 
                                              module: Module)
                                             (implicit m: Manifest[T]) = {
    module.getDefn(name) match {
      case Some(defn) if m.erasure.isInstance(defn) => Nil
      case Some(defn) => {
        List(InappropriateSymbolException(name,
                                          location,
                                          defn.location,
                                          humanReadableClassName[T]))
      }
      case None => List(UndefinedSymbolException(name, location))
    }
  }
}

final case class UnitType(override location: Location = Nowhere) extends Type(location) {
  def defaultValue = UnitValue(location)

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
  def defaultValue = BooleanValue(false, location)

  def isNumeric = false

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

  def defaultValue = {
    width match {
      case 8 => Int8Value(0, location)
      case 16 => Int16Value(0, location)
      case 32 => Int32Value(0, location)
      case 64 => Int64Value(0, location)
    }
  }

  def isNumeric = true

  override def equals(that: Any) = {
    that match {
      case IntType(w, _) if width == w => true
      case _ => false
    }
  }

  override def hashCode = List[Any]("int", width).foldLeft(0)(Utilities.hash _)

  override def toString = "#int" + width
}

final case class FloatType(width: Int, 
                           override location: Location = Nowhere)
  extends Type(location)
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException

  def defaultValue = throw new UnsupportedOperationException

  def isNumeric = true

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

  def defaultValue = NullValue()

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
  def defaultValue = NullValue()

  def isNumeric = false

  override def isPointer = true

  override def isSubtypeOf(ty: Type) = ty.isInstanceOf[NullType] || ty.isInstanceOf[PointerType]

  override def equals(that: Any) = that.isInstanceOf[NullType]

  override def hashCode = hash(0, "null")

  override def toString = "#null"
}

final case class UniversalType(typeParameters: List[Symbol],
                               baseType: Type,
                               override location: Location = Nowhere)
  extends Type(location)
{
  override def validate(module: Module) = {
    val errors = for (paramName <- typeParameters)
      yield validateName[TypeParameter](paramName, module)
    errors.flatMap(_.toList)
  }

  def defaultValue = throw new UnsupportedOperationException

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case UniversalType(pts, b, _) if typeParameters.sameElements(pts) && baseType == b => true
      case _ => false
    }
  }

  override def hashCode = { 
    val parts = "universal" :: baseType :: typeParameters.asInstanceOf[List[Any]]
    parts.foldLeft(0)(hash _)
  }

  override def toString = {
    "forall [" + typeParameters.mkString(", ") + "] . " + baseType
  }
}

final case class ExistentialType(typeParameters: List[Symbol],
                                 baseType: Type,
                                 override location: Location = Nowhere)
  extends Type(location)
{
  override def validate(module: Module) = {
    val errors = for (paramName <- typeParameters) 
      yield validateName[TypeParameter](paramName, module)
    errors.flatMap(_.toList)
  }

  def defaultValue = throw new UnsupportedOperationException

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case ExistentialType(pts, b, _) 
      if typeParameters.sameElements(pts) && baseType == b => true
      case _ => false
    }
  }

  override def hashCode = {
    val parts = "existential" :: baseType :: typeParameters.asInstanceOf[List[Any]]
    parts.foldLeft(0)(hash _)
  }

  override def toString = {
    "forsome [" + typeParameters.mkString(", ") + "] . " + baseType
  }
}

final case class VariableType(name: Symbol, 
                              override location: Location = Nowhere) 
  extends Type(location)
{
  override def validate(module: Module) = validateName[TypeParameter](name, module)

  def defaultValue = throw new UnsupportedOperationException

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case VariableType(n, _) if name == n => true
      case _ => false
    }
  }

  override def hashCode = List[Any]("variable", name).foldLeft(0)(hash _)

  override def toString = name.toString
}

final case class ArrayType(elementType: Type, 
                           override location: Location = Nowhere)
  extends Type(location)
{
  def defaultValue = throw new UnsupportedOperationException

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case ArrayType(e, _) if elementType == e => true
      case _ => false
    }
  }

  override def hashCode = List("array", elementType).foldLeft(0)(hash _)

  override def toString = "Array[" + elementType + "]"
}

final case class FunctionType(returnType: Type,
                              parameterTypes: List[Type], 
                              override location: Location = Nowhere)
  extends Type(location)
{
  def defaultValue = throw new UnsupportedOperationException

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

final case class ClassType(className: Symbol,
                           typeArguments: List[Type] = Nil,
                           override location: Location = Nowhere)
  extends Type(location)
{
  override def validate(module: Module) = validateName[Class](className, module)

  def defaultValue = throw new UnsupportedOperationException

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case ClassType(n, args, _)
      if className == n && typeArguments.sameElements(args) => true
      case _ => false
    }
  }

  override def hashCode = {
    val parts = "class" :: className :: typeArguments.asInstanceOf[List[Any]]
    parts.foldLeft(0)(hash _)
  }
}

final case class InterfaceType(interfaceName: Symbol,
                               typeArguments: List[Type] = Nil,
                               override location: Location = Nowhere)
  extends Type(location)
{
  override def validate(module: Module) = validateName[Interface](interfaceName, module)

  def defaultValue = throw new UnsupportedOperationException

  def isNumeric = false

  override def equals(that: Any) = {
    that match {
      case InterfaceType(n, args, _)
      if interfaceName == n && typeArguments.sameElements(args) => true
      case _ => false
    }
  }

  override def hashCode = {
    val parts = "interface" :: interfaceName :: typeArguments.asInstanceOf[List[Any]]
    parts.foldLeft(0)(hash _)
  }
}
