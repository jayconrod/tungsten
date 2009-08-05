package com.jayconrod.tungsten

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

  override val hashCode = Utilities.hash(0, "unit")

  override val toString = "()"
}
