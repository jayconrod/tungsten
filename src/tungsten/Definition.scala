package tungsten

abstract class Definition(val name: Symbol, val location: Location = Nowhere) {
  def toString: String
}
