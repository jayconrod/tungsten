package tungsten

sealed abstract class Value(val location: Location) {
  def equals(that: Any): Boolean
  def hashCode: Int
  def toString: String
}

