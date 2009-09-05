package tungsten

sealed abstract class Token(val location: Location)

final case class ErrorToken(msg: String, loc: Location) extends Token(loc)
final case class ReservedToken(val text: String, loc: Location) extends Token(loc)
