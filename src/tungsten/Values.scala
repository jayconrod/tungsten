package tungsten

sealed abstract class Value(location: Location) extends TungstenObject(location) {
  def ty(module: Module): Type
}

final case class UnitValue(override location: Location = Nowhere) extends Value(location) {
  def ty(module: Module): UnitType = UnitType(location)
}

final case class Int8Value(value: Byte, override location: Location = Nowhere) 
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(8, location)
}

final case class Int16Value(value: Short, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(16, location)
}

final case class Int32Value(value: Int, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(32, location)
}

final case class Int64Value(value: Long, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(64, location)
}

final case class DefinedValue(value: Symbol, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module) = {
    val defn = module.get(value)
    assert(defn.isDefined)
    defn.get.asInstanceOf[TypedDefinition].ty(module)
  }
}

