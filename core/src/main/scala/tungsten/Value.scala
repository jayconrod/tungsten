package tungsten

import Utilities._

sealed abstract class Value {
  def ty(module: Module): Type
 
  def validateComponents(module: Module, location: Location): List[CompileException] = Nil

  final def validateType(expectedType: Type, 
                         module: Module,
                         location: Location): List[CompileException] = 
  {
    val actualType = ty(module)
    if (actualType != expectedType)
      List(TypeMismatchException(actualType, expectedType, location))
    else
      Nil
  }
}

final case object UnitValue
  extends Value
{
  def ty(module: Module) = UnitType

  override def toString = "()"
}

final case class BooleanValue(value: Boolean)
  extends Value
{
  def ty(module: Module) = BooleanType

  override def toString = if (value) "#true" else "#false"
}

final case class Int8Value(value: Byte)
  extends Value
{
  def ty(module: Module): IntType = IntType(8)

  override def toString = value + "b"
}

final case class Int16Value(value: Short)
  extends Value
{
  def ty(module: Module): IntType = IntType(16)

  override def toString = value + "s"
}

final case class Int32Value(value: Int)
  extends Value
{
  def ty(module: Module): IntType = IntType(32)

  override def toString = value.toString
}

final case class Int64Value(value: Long)
  extends Value
{
  def ty(module: Module): IntType = IntType(64)

  override def toString = value + "L"
}

final case class Float32Value(value: Float)
  extends Value
{
  def ty(module: Module): FloatType = FloatType(32)

  override def toString = value + "f"
}

final case class Float64Value(value: Double)
  extends Value
{
  def ty(module: Module): FloatType = FloatType(64)

  override def toString = value.toString
}

final case object NullValue
  extends Value
{
  def ty(module: Module): Type = NullType

  override def toString = "#null"
}

final case class ArrayValue(elementType: Type,
                            elements: List[Value])
  extends Value
{
  def ty(module: Module) = ArrayType(Some(elements.size), elementType)

  override def validateComponents(module: Module, location: Location) = {
    def validateElementComponents(element: Value) = {
      stage(element.validateComponents(module, location),
            element.validateType(elementType, module, location))
    }
    
    stage(elementType.validate(module, location),
          elements.flatMap(validateElementComponents _))
  }

  override def toString = {
    val elementsStr = elements.mkString(", ")
    "[%s: %s]".format(elementType, elementsStr)
  }
}      

final case class StructValue(structName: Symbol,
                             fields: List[Value])
  extends Value
{
  def ty(module: Module) = StructType(structName)

  override def validateComponents(module: Module, location: Location) = {
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
        f.validateType(t, module, location)
      }
    }

    stage(module.validateName[Struct](structName, location),
          validateFieldCount,
          fields.flatMap(_.validateComponents(module, location)),
          validateFieldTypes)
  }

  override def toString = {
    val fieldsStr = fields.mkString(", ")
    "{%s: %s}".format(structName, fieldsStr)
  }
}

final case class DefinedValue(value: Symbol)
  extends Value
{
  def ty(module: Module) = {
    module.getDefn(value) match {
      case Some(Global(_, t, _, _)) => PointerType(t)
      case Some(Parameter(_, t, _)) => t
      case Some(inst: Instruction) => inst.ty(module)
      case _ => throw new RuntimeException("symbol " + value + " cannot be used as a value")
    }
  }

  override def validateComponents(module: Module, location: Location) = {
    module.getDefn(value) match {
      case Some(_: Global) | Some(_: Parameter) | Some(_: Instruction) => Nil
      case Some(defn) => {
        List(InappropriateSymbolException(value, 
                                          defn.getLocation,
                                          defn.getLocation,
                                          "global, parameter, or instruction"))
      }
      case None => List(UndefinedSymbolException(value, location))
    }
  }

  override def toString = value.toString
}
