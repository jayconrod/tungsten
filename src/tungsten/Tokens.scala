package tungsten

sealed abstract class Token {
  var location: Location = Nowhere
}

final case class ErrorToken(msg: String) extends Token
final case class ReservedToken(val text: String) extends Token
