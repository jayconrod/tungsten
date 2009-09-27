package tungsten

sealed abstract class Value(location: Location) extends TungstenObject(location)

final case class UnitValue(override location: Location) extends Value(location)
