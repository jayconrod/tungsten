package com.jayconrod.tungsten

import Utilities._

abstract sealed class Type(val location: Location) {
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
