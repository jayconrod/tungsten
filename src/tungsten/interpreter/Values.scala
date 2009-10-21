package tungsten.interpreter

import tungsten.{Parameter, Instruction, Function}

abstract class Value

final case object UnitValue extends Value

final case class Int8Value(value: Byte) extends Value

final case class Int16Value(value: Short) extends Value

final case class Int32Value(value: Int) extends Value

final case class Int64Value(value: Long) extends Value

final case class FunctionValue(value: Function) extends Value

object Value {
  def eval(value: tungsten.Value, env: Environment): Value = {
    value match {
      case tungsten.UnitValue(_) => UnitValue
      case tungsten.Int8Value(v, _) => Int8Value(v)
      case tungsten.Int16Value(v, _) => Int16Value(v)
      case tungsten.Int32Value(v, _) => Int32Value(v)
      case tungsten.Int64Value(v, _) => Int64Value(v)
      case tungsten.DefinedValue(v, _) => env.module.getDefn(v) match {
        case Some(p: Parameter) => env.state.values(v)
        case Some(i: Instruction) => env.state.values(v)
        case Some(f: Function) => FunctionValue(f)
        case _ => throw new UnsupportedOperationException
        // TODO: other values
      }        
    }
  }
}
