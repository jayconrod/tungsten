package tungsten

sealed abstract class Value(location: Location) extends TungstenObject(location) {
  def equals(that: Any): Boolean
  def hashCode: Int
  def toString: String
}

