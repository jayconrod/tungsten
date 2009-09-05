package tungsten

sealed abstract class Token
final case class ErrorToken(msg: String) extends Token
