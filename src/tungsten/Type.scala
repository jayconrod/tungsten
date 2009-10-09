package tungsten

import Utilities._

abstract sealed class Type(location: Location) 
  extends TungstenObject(location)
{
  def equals(that: Any): Boolean
  def hashCode: Int
  def toString: String
}

final case class UnitType(loc: Location = Nowhere) extends Type(loc) {
  override def equals(that: Any) = {
    that match {
      case UnitType(_) => true
      case _ => false
    }
  }

  override def hashCode = hash(0, "unit")

  override def toString = "unit"
}

final case class IntType(width: Int, loc: Location = Nowhere) extends Type(loc) {
  if (width < 1 || !isPowerOf2(width) || width > 64)
    throw new IllegalArgumentException

  override def equals(that: Any) = {
    that match {
      case IntType(w, _) if width == w => true
      case _ => false
    }
  }

  override def hashCode = List[Any]("int", width).foldLeft(0)(Utilities.hash _)

  override def toString = "int" + width
}

final case class FloatType(width: Int, loc: Location = Nowhere) extends Type(loc) {
  if (width != 32 && width != 64)
    throw new IllegalArgumentException

  override def equals(that: Any) = {
    that match {
      case FloatType(w, _) if width == w => true
      case _ => false
    }
  }

  override def hashCode = List[Any]("float", width).foldLeft(0)(hash _)

  override def toString = "float" + width
}

final case class UniversalType(parameterTypes: List[Symbol],
                               baseType: Type,
                               loc: Location = Nowhere)
  extends Type(loc)
{
  override def equals(that: Any) = {
    that match {
      case UniversalType(pts, b, _) if parameterTypes.sameElements(pts) && baseType == b => true
      case _ => false
    }
  }

  override def hashCode = { 
    val parts = "universal" :: baseType :: parameterTypes.asInstanceOf[List[Any]]
    parts.foldLeft(0)(hash _)
  }

  override def toString = {
    "forall [" + parameterTypes.mkString(", ") + "] . " + baseType
  }
}

final case class ExistentialType(parameterTypes: List[Symbol],
                                 baseType: Type,
                                 loc: Location = Nowhere)
  extends Type(loc)
{
  override def equals(that: Any) = {
    that match {
      case ExistentialType(pts, b, _) 
      if parameterTypes.sameElements(pts) && baseType == b => true
      case _ => false
    }
  }

  override def hashCode = {
    val parts = "existential" :: baseType :: parameterTypes.asInstanceOf[List[Any]]
    parts.foldLeft(0)(hash _)
  }

  override def toString = {
    "forsome [" + parameterTypes.mkString(", ") + "] . " + baseType
  }
}

final case class VariableType(name: Symbol, loc: Location = Nowhere) extends Type(loc) {
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
                           loc: Location = Nowhere)
  extends Type(loc)
{
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
                              loc: Location = Nowhere)
  extends Type(loc)
{
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
                           loc: Location = Nowhere)
  extends Type(loc)
{
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
                               loc: Location = Nowhere)
  extends Type(loc)
{
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

final case class NullType(loc: Location = Nowhere) extends Type(loc) {
  override def equals(that: Any) = {
    that match {
      case NullType(_) => true
      case _ => false
    }
  }

  override def hashCode = hash(0, "null")

  override def toString = "Null"
}
  
