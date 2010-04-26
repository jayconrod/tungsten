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

final case class CharValue(value: Char)
  extends Value
{
  def ty(module: Module) = CharType

  def isPrintable: Boolean = Utilities.charIsPrintable(value)

  override def toString = {
    val valueStr = if (isPrintable) 
      value.toString 
    else
      "\\%04d".format(value.toInt)
    "'" + valueStr + "'"
  }
}

final case class StringValue(value: String)
  extends Value
{
  def ty(module: Module) = StringType

  override def toString = {
    val buffer = new StringBuffer
    buffer.append("\"")
    for (ch <- value) {
      if (charIsPrintable(ch))
        buffer.append(ch)
      else
        buffer.append("%04d".format(ch.toInt))
    }
    buffer.append("\"")
    buffer.toString
  }
}      

final case class IntValue(value: Long, width: Int)
  extends Value
{
  if (width < 8 || !isPowerOf2(width) || width > 64)
    throw new IllegalArgumentException

  def ty(module: Module) = IntType(width)

  override def toString = {
    val suffix = width match {
      case 8 => "b"
      case 16 => "s"
      case 32 => ""
      case _ => "L"
    }
    value + suffix
  }
}

final case class FloatValue(value: Double, width: Int) 
  extends Value
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException

  def ty(module: Module) = FloatType(width)

  override def toString = {
    val suffix = if (width == 32) "f" else ""
    value + suffix
  }
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
  var ty: Type = UnitType

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
                                          "global, parameter, or instruction"))
      }
      case None => List(UndefinedSymbolException(value, location))
    }
  }

  override def toString = value.toString
}
object DefinedValue {
  def apply(value: Symbol, ty: Type): DefinedValue = {
    val dv = DefinedValue(value)
    dv.ty = ty
    dv
  }
}
