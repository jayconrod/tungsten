package tungsten

sealed abstract class Value(location: Location) extends TungstenObject(location)

final case class UnitValue(override location: Location) extends Value(location)

final case class Int8Value(value: Byte, override location: Location) extends Value(location)

final case class Int16Value(value: Short, override location: Location) extends Value(location)

final case class Int32Value(value: Int, override location: Location) extends Value(location)

final case class Int64Value(value: Long, override location: Location) extends Value(location)

final case class DefinedValue(value: Definition, override location: Location)
  extends Value(location)
