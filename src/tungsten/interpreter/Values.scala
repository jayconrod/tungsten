package tungsten.interpreter

import tungsten.{Parameter, Instruction, Function}

sealed class IValue

final case object UnitValue extends IValue

final case class BooleanValue(value: Boolean) extends IValue
{
  def &(r: BooleanValue) = BooleanValue(value & r.value)
  def ^(r: BooleanValue) = BooleanValue(value ^ r.value)
  def |(r: BooleanValue) = BooleanValue(value | r.value)
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

final class ScalarReference(var value: IValue) extends Reference {
  override def toString = "ScalarReference(" + value + ")"
}
object ScalarReference {
  def apply(value: IValue) = new ScalarReference(value)
}

final class ArrayIndexReference(val array: ArrayValue, val index: Int64Value) 
  extends Reference
{
  private val intIndex = index.value.asInstanceOf[Int]

  def value = array.value(intIndex)

  def value_=(v: IValue) = array.value(intIndex) = v

  override def toString = "ArrayReference(" + value + ")"
}
object ArrayIndexReference {
  def apply(array: ArrayValue, index: Int64Value) = {
    new ArrayIndexReference(array, index)
  }
}

case object NullReference extends Reference {
  def value = throw new RuntimeException("null pointer dereferenced")

  def value_=(v: IValue) = throw new RuntimeException("null pointer dereferenced")
}

final case class ArrayValue(value: Array[IValue]) extends IValue {
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

  override def hashCode = super.hashCode
}

final case class FunctionValue(value: Function) extends IValue
