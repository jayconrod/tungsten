package tungsten.interpreter

import tungsten.{Parameter, Instruction, Function}

sealed class IValue

final case object UnitValue extends IValue {
  override def toString = "()"
}

final case class BooleanValue(value: Boolean) extends IValue
{
  def &(r: BooleanValue) = BooleanValue(value & r.value)
  def ^(r: BooleanValue) = BooleanValue(value ^ r.value)
  def |(r: BooleanValue) = BooleanValue(value | r.value)
  override def toString = if (value) "#true" else "#false"
}

final case class Int8Value(value: Byte) extends IValue {
  def *(r: Int8Value) = Int8Value((value * r.value).asInstanceOf[Byte])
  def /(r: Int8Value) = Int8Value((value / r.value).asInstanceOf[Byte])
  def %(r: Int8Value) = Int8Value((value % r.value).asInstanceOf[Byte])
  def +(r: Int8Value) = Int8Value((value + r.value).asInstanceOf[Byte])
  def -(r: Int8Value) = Int8Value((value - r.value).asInstanceOf[Byte])
  def <<(r: Int8Value) = Int8Value((value << r.value).asInstanceOf[Byte])
  def >>(r: Int8Value) = Int8Value((value >> r.value).asInstanceOf[Byte])
  def >>>(r: Int8Value) = Int8Value((value >>> r.value).asInstanceOf[Byte])
  def &(r: Int8Value) = Int8Value((value & r.value).asInstanceOf[Byte])
  def ^(r: Int8Value) = Int8Value((value ^ r.value).asInstanceOf[Byte])
  def |(r: Int8Value) = Int8Value((value | r.value).asInstanceOf[Byte])
  def <(r: Int8Value) = BooleanValue(value < r.value)
  def <=(r: Int8Value) = BooleanValue(value <= r.value)
  def >(r: Int8Value) = BooleanValue(value > r.value)
  def >=(r: Int8Value) = BooleanValue(value >= r.value)
  override def toString = value + "b"
}

final case class Int16Value(value: Short) extends IValue {
  def *(r: Int16Value) = Int16Value((value * r.value).asInstanceOf[Short])
  def /(r: Int16Value) = Int16Value((value / r.value).asInstanceOf[Short])
  def %(r: Int16Value) = Int16Value((value % r.value).asInstanceOf[Short])
  def +(r: Int16Value) = Int16Value((value + r.value).asInstanceOf[Short])
  def -(r: Int16Value) = Int16Value((value - r.value).asInstanceOf[Short])
  def <<(r: Int16Value) = Int16Value((value << r.value).asInstanceOf[Short])
  def >>(r: Int16Value) = Int16Value((value >> r.value).asInstanceOf[Short])
  def >>>(r: Int16Value) = Int16Value((value >>> r.value).asInstanceOf[Short])
  def &(r: Int16Value) = Int16Value((value & r.value).asInstanceOf[Short])
  def ^(r: Int16Value) = Int16Value((value ^ r.value).asInstanceOf[Short])
  def |(r: Int16Value) = Int16Value((value | r.value).asInstanceOf[Short])
  def <(r: Int16Value) = BooleanValue(value < r.value)
  def <=(r: Int16Value) = BooleanValue(value <= r.value)
  def >(r: Int16Value) = BooleanValue(value > r.value)
  def >=(r: Int16Value) = BooleanValue(value >= r.value)
  override def toString = value + "s"
}

final case class Int32Value(value: Int) extends IValue {
  def *(r: Int32Value) = Int32Value(value * r.value)
  def /(r: Int32Value) = Int32Value(value / r.value)
  def %(r: Int32Value) = Int32Value(value % r.value)
  def +(r: Int32Value) = Int32Value(value + r.value)
  def -(r: Int32Value) = Int32Value(value - r.value)
  def <<(r: Int32Value) = Int32Value(value << r.value)
  def >>(r: Int32Value) = Int32Value(value >> r.value)
  def >>>(r: Int32Value) = Int32Value(value >>> r.value)
  def &(r: Int32Value) = Int32Value(value & r.value)
  def ^(r: Int32Value) = Int32Value(value ^ r.value)
  def |(r: Int32Value) = Int32Value(value | r.value)
  def <(r: Int32Value) = BooleanValue(value < r.value)
  def <=(r: Int32Value) = BooleanValue(value <= r.value)
  def >(r: Int32Value) = BooleanValue(value > r.value)
  def >=(r: Int32Value) = BooleanValue(value >= r.value)
  override def toString = value.toString
}

final case class Int64Value(value: Long) extends IValue {
  def *(r: Int64Value) = Int64Value(value * r.value)
  def /(r: Int64Value) = Int64Value(value / r.value)
  def %(r: Int64Value) = Int64Value(value % r.value)
  def +(r: Int64Value) = Int64Value(value + r.value)
  def -(r: Int64Value) = Int64Value(value - r.value)
  def <<(r: Int64Value) = Int64Value(value << r.value)
  def >>(r: Int64Value) = Int64Value(value >> r.value)
  def >>>(r: Int64Value) = Int64Value(value >>> r.value)
  def &(r: Int64Value) = Int64Value(value & r.value)
  def ^(r: Int64Value) = Int64Value(value ^ r.value)
  def |(r: Int64Value) = Int64Value(value | r.value)
  def <(r: Int64Value) = BooleanValue(value < r.value)
  def <=(r: Int64Value) = BooleanValue(value <= r.value)
  def >(r: Int64Value) = BooleanValue(value > r.value)
  def >=(r: Int64Value) = BooleanValue(value >= r.value)
  override def toString = value + "L"
}

final case class Float32Value(value: Float) extends IValue {
  def *(r: Float32Value) = Float32Value(value * r.value)
  def /(r: Float32Value) = Float32Value(value / r.value)
  def %(r: Float32Value) = Float32Value(value % r.value)
  def +(r: Float32Value) = Float32Value(value + r.value)
  def -(r: Float32Value) = Float32Value(value - r.value)
  def <(r: Float32Value) = BooleanValue(value < r.value)
  def <=(r: Float32Value) = BooleanValue(value <= r.value)
  def >(r: Float32Value) = BooleanValue(value > r.value)
  def >=(r: Float32Value) = BooleanValue(value >= r.value)
  override def toString = value + "f"
}

final case class Float64Value(value: Double) extends IValue {
  def *(r: Float64Value) = Float64Value(value * r.value)
  def /(r: Float64Value) = Float64Value(value / r.value)
  def %(r: Float64Value) = Float64Value(value % r.value)
  def +(r: Float64Value) = Float64Value(value + r.value)
  def -(r: Float64Value) = Float64Value(value - r.value)
  def <(r: Float64Value) = BooleanValue(value < r.value)
  def <=(r: Float64Value) = BooleanValue(value <= r.value)
  def >(r: Float64Value) = BooleanValue(value > r.value)
  def >=(r: Float64Value) = BooleanValue(value >= r.value)
  override def toString = value.toString
}

sealed abstract class Reference extends IValue {
  var value: IValue
}
object Reference {
  def unapply(that: IValue): Option[IValue] = {
    if (that.isInstanceOf[Reference])
      Some(that.asInstanceOf[Reference].value)
    else
      None
  }
}

final case class ScalarReference(var value: IValue) extends Reference {
  override def toString = "ScalarReference(" + value + ")"
}

final case class ArrayIndexReference(val array: ArrayValue, val index: Int64Value) 
  extends Reference
{
  private val intIndex = index.value.asInstanceOf[Int]

  def value = array.value(intIndex)

  def value_=(v: IValue) = array.value(intIndex) = v

  override def toString = "ArrayReference(" + value + ")"
}

final case class StructIndexReference(val struct: StructValue, val index: Int64Value) 
  extends Reference
{
  private val intIndex = index.value.asInstanceOf[Int]

  def value = struct.value(intIndex)

  def value_=(v: IValue) = struct.value(intIndex) = v

  override def toString = "StructReference(" + value + ")"
}

case object NullReference extends Reference {
  def value = throw new RuntimeException("null pointer dereferenced")

  def value_=(v: IValue) = throw new RuntimeException("null pointer dereferenced")

  override def toString = "#null"
}

final case class ArrayValue(value: Array[IValue]) extends IValue {
  override def equals(that: Any) = {
    that match {
      case ArrayValue(v) if value.deep == v.deep => true
      case _ => false
    }
  }

  override def hashCode = hash("ArrayValue", value.deep)

  override def toString = value.mkString("[", ", ", "]")
}

final case class StructValue(value: Array[IValue]) extends IValue {
  override def equals(that: Any) = {
    that match {
      case StructValue(v) if value.deep == v.deep => true
      case _ => false
    }
  }

  override def hashCode = hash("StructValue", value.deep)

  override def toString = value.mkString("[", ", ", "]")
}

final case class FunctionValue(value: Function) extends IValue {
  override def toString = "<function " + value.name + ">"
}
