package tungsten

sealed abstract class Token {
  var location: Location = Nowhere
}

final case class ErrorToken(msg: String) extends Token {
  override def equals(that: Any) = that.isInstanceOf[ErrorToken]
  override val hashCode = "ErrorToken".hashCode
}
final case class ReservedToken(val text: String) extends Token
final case class SymbolToken(val symbol: Symbol) extends Token
final case class LocationToken(val loc: Location) extends Token
