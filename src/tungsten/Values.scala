package tungsten

import Utilities._

sealed abstract class Value(location: Location) extends TungstenObject(location) {
  def ty(module: Module): Type
 
  def validateComponents(module: Module): List[CompileException] = Nil

  final def validateType(expectedType: Type, module: Module): List[CompileException] = {
    val actualType = ty(module)
    if (actualType != expectedType)
      List(TypeMismatchException(actualType, expectedType, location))
    else
      Nil
  }
}

final case class UnitValue(override location: Location = Nowhere) extends Value(location) {
  def ty(module: Module): UnitType = UnitType(location)

  override def toString = "()"
}

final case class BooleanValue(value: Boolean, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): BooleanType = BooleanType(location)

  override def toString = if (value) "#true" else "#false"
}

final case class Int8Value(value: Byte, override location: Location = Nowhere) 
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(8, location)

  override def toString = value + "b"
}

final case class Int16Value(value: Short, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(16, location)

  override def toString = value + "s"
}

final case class Int32Value(value: Int, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(32, location)

  override def toString = value.toString
}

final case class Int64Value(value: Long, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): IntType = IntType(64, location)

  override def toString = value + "L"
}

final case class Float32Value(value: Float, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): FloatType = FloatType(32, location)

  override def toString = value + "f"
}

final case class Float64Value(value: Double, override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): FloatType = FloatType(64, location)

  override def toString = value.toString
}

final case class NullValue(override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module): NullType = NullType(location)

  override def toString = "#null"
}

final case class ArrayValue(elementType: Type,
                            elements: List[Value],
                            override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module) = ArrayType(Some(elements.size), elementType, location)

  override def validateComponents(module: Module) = {
    def validateElementComponents(element: Value) = {
      stage(element.validateComponents(module),
            element.validateType(elementType, module))
    }
    
    stage(elementType.validate(module),
          elements.flatMap(validateElementComponents _))
  }

  override def toString = {
    val elementsStr = elements.mkString(", ")
    val locationStr = if (location == Nowhere) "" else location.toString
    "[%s: %s]%s".format(elementType, elementsStr, locationStr)
  }
}      

final case class StructValue(structName: Symbol,
                             fields: List[Value],
                             override location: Location = Nowhere)
  extends Value(location)
{
  def ty(module: Module) = StructType(structName, location)

  override def validateComponents(module: Module) = {
    def validateFieldCount = {
      val struct = module.getStruct(structName)
      if (fields.size == struct.fields.size)
        Nil
      else
        List(FieldCountException(structName, fields.size, struct.fields.size, location))
    }

    def validateFieldTypes = {
      val struct = module.getStruct(structName)
      val fieldTypes = module.getFields(struct.fields).map(_.ty)
      (fields zip fieldTypes) flatMap { ft =>
        val (f, t) = ft
        f.validateType(t, module)
      }
    }

    stage(module.validateName[Struct](structName, location),
          validateFieldCount,
          fields.flatMap(_.validateComponents(module)),
          validateFieldTypes)
  }

  override def toString = {
    val fieldsStr = fields.mkString(", ")
    val locationStr = if (location == Nowhere) "" else location.toString
    "{%s: %s}%s".format(structName, fieldsStr, locationStr)
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

  override def validateComponents(module: Module) = {
    module.getDefn(value) match {
      case Some(_: Global) | Some(_: Parameter) | Some(_: Instruction) => Nil
      case Some(defn) => {
        List(InappropriateSymbolException(value, 
                                          location,
                                          defn.location,
                                          "global, parameter, or instruction"))
      }
      case None => List(UndefinedSymbolException(value, location))
    }
  }

  override def toString = value.toString
}
