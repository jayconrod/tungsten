package tungsten

import Utilities._

sealed abstract class Value 
  extends Copying[Value]
{
  def ty: Type

  /** Checks that the symbols referred to by this value correspond to the correct type of 
   *  definitions. For instance, a struct value includes a symbol which must name a struct.
   *  This method is recursive for values which contain other values.
   */
  def validateComponents(module: Module, location: Location): List[CompileException] = Nil

  /** Checks that this value satisfies the invariants of its type. For instance, a struct 
   *  value must have the correct number of fields, and they must be of the correct type.
   *  This should be called after valdiateComponents is called for all definitions and only
   *  if there are no errors. This method is recursive for values which contain other values.
   */
  def validate(module: Module, location: Location): List[CompileException] = Nil

  /** Checks that this value is of a given type */
  final def validateType(expectedType: Type, location: Location): List[CompileException] =
  {
    if (ty != expectedType)
      List(TypeMismatchException(ty, expectedType, location))
    else
      Nil
  }
}

final case object UnitValue
  extends Value
{
  def ty = UnitType

  override def toString = "()"
}

final case class BooleanValue(value: Boolean)
  extends Value
{
  def ty = BooleanType

  override def toString = if (value) "#true" else "#false"
}

final case class CharValue(value: Char)
  extends Value
{
  def ty = CharType

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
  def ty = StringType

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

  def ty = IntType(width)

  override def validate(module: Module, location: Location): List[CompileException] =
  {
    if (value < ty.minValue || value > ty.maxValue)
      List(IntegerRangeException(value, width, location))
    else
      Nil
  }

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

  def ty = FloatType(width)

  override def toString = {
    val suffix = if (width == 32) "f" else ""
    value + suffix
  }
}

final case object NullValue
  extends Value
{
  def ty = NullType

  override def toString = "#null"
}

final case class ArrayValue(elementType: Type,
                            elements: List[Value])
  extends Value
{
  def ty = ArrayType(Some(elements.size), elementType)

  override def validateComponents(module: Module, location: Location) = {
    elementType.validate(module, location) ++ 
      elements.flatMap(_.validateComponents(module, location))
  }

  override def validate(module: Module, location: Location): List[CompileException] = {
    elements.flatMap(_.validate(module, location)) ++
      elements.flatMap(_.validateType(elementType, location))
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
  def ty = StructType(structName)

  override def validateComponents(module: Module, location: Location) = {
    module.validateName[Struct](structName, location)
  }

  override def validate(module: Module, location: Location): List[CompileException] = {
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
        f.validateType(t, location)
      }
    }

    fields.flatMap(_.validate(module, location)) ++
      stage(validateFieldCount,
            validateFieldTypes)
  }

  override def toString = {
    val fieldsStr = fields.mkString(", ")
    "{%s: %s}".format(structName, fieldsStr)
  }
}

final case class DefinedValue(value: Symbol, ty: Type)
  extends Value
{
  override def validateComponents(module: Module, location: Location) = {
    def validateName = {
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

    validateName ++ ty.validate(module, location)
  }

  override def toString = value.toString
}
