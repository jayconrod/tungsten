package tungsten

sealed abstract class Value(location: Location) extends TungstenObject(location) {
  def ty(module: Module): Type
 
  def validateType(module: Module, expectedType: Type): List[CompileException] = {
    val actualType = ty(module)
    if (actualType != expectedType)
      List(TypeMismatchException(actualType.toString, expectedType.toString, location))
    else
      Nil
  }
}

final case class UnitValue(override location: Location = Nowhere) extends Value(location) {
  def ty(module: Module): UnitType = UnitType(location)
}

final case class BooleanValue(value: Boolean, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): BooleanType = BooleanType(location)
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

final case class Float32Value(value: Float, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): FloatType = FloatType(32, location)
}

final case class Float64Value(value: Double, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): FloatType = FloatType(64, location)
}

final case class NullValue(override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): NullType = NullType(location)
}

final case class ArrayValue(elementType: Type,
                            elements: List[Value],
                            override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module) = ArrayType(Some(elements.size), elementType, location)

  override def validateType(module: Module, expectedType: Type) = {
    val actualType = ty(module)
    val errors: List[CompileException] = if (actualType != expectedType)
      List(TypeMismatchException(actualType.toString, expectedType.toString, location))
    else
      Nil

    elements.foldLeft(errors) { (errors, elem) => 
      elem.validateType(module, elementType) ++ errors
    }
  }
}      

final case class DefinedValue(value: Symbol, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module) = {
    module.getDefn(value) match {
      case Some(Global(_, t, _, _)) => PointerType(t, location)
      case Some(Parameter(_, t, _)) => t
      case Some(inst: Instruction) => inst.ty(module)
      case _ => throw new RuntimeException("symbol " + value + " cannot be used as a value")
    }
  }
}
