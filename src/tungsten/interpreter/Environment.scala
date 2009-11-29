package tungsten.interpreter

import scala.collection.immutable.Stack
import tungsten._
import tungsten.BinaryOperator._
import tungsten.RelationalOperator._
import Value._

final class Environment(val module: Module) {
  val init = Array[Instruction](StaticCallInstruction(new Symbol("init"), 
                                                      new Symbol("main"),
                                                      Nil),
                                IntrinsicCallInstruction(new Symbol("exit"), 
                                                      Intrinsic.EXIT,
                                                      List(tungsten.Int32Value(0))))

  var stack = new Stack[State]

  var state = new State(new Symbol("init"), init, 0, Map[Symbol, Value]())

  var globalState = module.definitions.valueIterable.foldLeft(Map[Symbol, Value]()) { (st, d) =>
    d match {
      case Global(name, _, Some(value), _) => st + (name -> Value.eval(value, this))
      case Global(name, ty, None, _) => st + (name -> Value.eval(ty.defaultValue, this))
      case _ => st
    }
  }

  var returnCode = 0

  def run = {
    while (state != null)
      step
    returnCode
  }

  def step = {
    val oldState = state
    val result = eval(state.inst)
    if (state == oldState)  // normal non-control instruction
      state = state.add(result).next
  }

  def eval(inst: Instruction): Value = {
    inst match {
      case AssignInstruction(name, value, _) => Value.eval(value, this)
      case BinaryOperatorInstruction(name, op, left, right, _) => {
        val l = Value.eval(left, this)
        val r = Value.eval(right, this)
        evalBinop(op, l, r)
      }
      case BranchInstruction(name, bbName, args, _) => {
        val iargs = args.map(Value.eval(_, this))
        val block = module.get[Block](bbName).get
        branch(block, iargs)
        state = state.add(name, UnitValue)
        UnitValue
      }
      case ConditionalBranchInstruction(name, cond, trueTarget, falseTarget, args, _) => {
        val BooleanValue(icond) = Value.eval(cond, this)
        val iargs = args.map(Value.eval(_, this))
        val target = if (icond) trueTarget else falseTarget
        val block = module.get[Block](target).get
        branch(block, iargs)
        UnitValue
      }        
      case GlobalLoadInstruction(name, globalName, _) => globalState(globalName)
      case GlobalStoreInstruction(name, globalName, value, _) => {
        globalState = globalState + (globalName -> Value.eval(value, this))
        UnitValue
      }
      case IndirectCallInstruction(name, target, arguments, _) => {
        val function = Value.eval(target, this).asInstanceOf[FunctionValue].value
        val args = arguments.map(Value.eval(_, this))
        call(function, args)
        UnitValue
      }
      case IntrinsicCallInstruction(name, intrinsic, arguments, _) => {
        import tungsten.Intrinsic._
        intrinsic match {
          case EXIT => {
            returnCode = Value.eval(arguments(0), this).asInstanceOf[Int32Value].value
            state = null
          }
        }
        UnitValue
      }
      case LoadInstruction(_, pointer, _) => {
        val scalar = Value.eval(pointer, this).asInstanceOf[ScalarReferenceValue]
        scalar.value
      }
      case LoadElementInstruction(_, base, indices, _) => {
        def loadElement(ptr: Value, indices: List[Value]): Value = {
          indices match {
            case Nil => ptr
            case i :: is => {
              val index = i.asInstanceOf[Int64Value].value.asInstanceOf[Int]
              ptr match {
                case ArrayValue(array) => array(index)
                case _ => throw new RuntimeException("indexed non-array value")
              }
            }
          }
        }
        loadElement(Value.eval(base, this), indices.map(Value.eval(_, this)))
      }
      case RelationalOperatorInstruction(name, op, left, right, _) => {
        val l = Value.eval(left, this)
        val r = Value.eval(right, this)
        evalRelop(op, l, r)
      }
      case ReturnInstruction(_, v, _) => {
        val ret = Value.eval(v, this)
        state = stack.head
        stack = stack.pop
        state = state.add(state.inst.name, ret).next
        UnitValue   // gets assigned to the actual return instruction
      }
      case StackAllocateInstruction(_, ty, _) => {
        val elementType = ty.asInstanceOf[PointerType].elementType
        val defaultValue = Value.eval(elementType.defaultValue, this)
        new ScalarReferenceValue(defaultValue)
      }
      case StaticCallInstruction(name, target, arguments, _) => {
        val function = module.get[Function](target).get
        val args = arguments.map(Value.eval(_, this))
        call(function, args)
        UnitValue
      }
      case StoreInstruction(_, pointer, value, _) => {
        val scalar = Value.eval(pointer, this).asInstanceOf[ScalarReferenceValue]
        val v = Value.eval(value, this)
        scalar.value = v
        UnitValue
      }
      case StoreElementInstruction(_, base, indices, value, _) => {
        val v = Value.eval(value, this)
        def storeElement(ptr: Value, indices: List[Value]): Unit = {
          indices match {
            case i :: Nil => {
              val index = i.asInstanceOf[Int64Value].value.asInstanceOf[Int]
              ptr match {
                case ArrayValue(array) => array(index) = v
                case _ => throw new RuntimeException("indexed non-array value")
              }
            }
            case i :: is => {
              val index = i.asInstanceOf[Int64Value].value.asInstanceOf[Int]
              ptr match {
                case ArrayValue(array) => storeElement(array(index), is)
                case _ => throw new RuntimeException("indexed non-array value")
              }
            }
            case _ => throw new RuntimeException("can't have empty index list")
          }
        }
        storeElement(Value.eval(base, this), indices.map(Value.eval(_, this)))
        UnitValue
      }
      case UpcastInstruction(_, value, _, _) => Value.eval(value, this)
    }
  }

  def setArguments(paramNames: List[Symbol], arguments: List[Value]) = {
    assert(paramNames.length == arguments.length)
    state = (paramNames zip arguments).foldLeft(state) { (st, arg) =>
      val (k, v) = arg
      st.add(k, v)
    }
  }         

  def branch(block: Block, arguments: List[Value]) = {
    state = new State(block, module)
    setArguments(block.parameters, arguments)
  }

  def call(function: Function, arguments: List[Value]) = {
    stack = stack.push(state)
    state = new State(function, module)
    setArguments(function.parameters, arguments)
  }

  def evalBinop(op: BinaryOperator, left: Value, right: Value) = {
    (op, left, right) match {
      case (MULTIPLY, Int8Value(l), Int8Value(r)) => Int8Value((l * r).asInstanceOf[Byte])
      case (MULTIPLY, Int16Value(l), Int16Value(r)) => Int16Value((l * r).asInstanceOf[Short])
      case (MULTIPLY, Int32Value(l), Int32Value(r)) => Int32Value(l * r)
      case (MULTIPLY, Int64Value(l), Int64Value(r)) => Int64Value(l * r)
      case (MULTIPLY, Float32Value(l), Float32Value(r)) => Float32Value(l * r)
      case (MULTIPLY, Float64Value(l), Float64Value(r)) => Float64Value(l * r)

      case (DIVIDE, Int8Value(l), Int8Value(r)) => Int8Value((l / r).asInstanceOf[Byte])
      case (DIVIDE, Int16Value(l), Int16Value(r)) => Int16Value((l / r).asInstanceOf[Short])
      case (DIVIDE, Int32Value(l), Int32Value(r)) => Int32Value(l / r)
      case (DIVIDE, Int64Value(l), Int64Value(r)) => Int64Value(l / r)
      case (DIVIDE, Float32Value(l), Float32Value(r)) => Float32Value(l / r)
      case (DIVIDE, Float64Value(l), Float64Value(r)) => Float64Value(l / r)

      case (REMAINDER, Int8Value(l), Int8Value(r)) => Int8Value((l % r).asInstanceOf[Byte])
      case (REMAINDER, Int16Value(l), Int16Value(r)) => Int16Value((l % r).asInstanceOf[Short])
      case (REMAINDER, Int32Value(l), Int32Value(r)) => Int32Value(l % r)
      case (REMAINDER, Int64Value(l), Int64Value(r)) => Int64Value(l % r)
      case (REMAINDER, Float32Value(l), Float32Value(r)) => Float32Value(l % r)
      case (REMAINDER, Float64Value(l), Float64Value(r)) => Float64Value(l % r)

      case (ADD, Int8Value(l), Int8Value(r)) => Int8Value((l + r).asInstanceOf[Byte])
      case (ADD, Int16Value(l), Int16Value(r)) => Int16Value((l + r).asInstanceOf[Short])
      case (ADD, Int32Value(l), Int32Value(r)) => Int32Value(l + r)
      case (ADD, Int64Value(l), Int64Value(r)) => Int64Value(l + r)
      case (ADD, Float32Value(l), Float32Value(r)) => Float32Value(l + r)
      case (ADD, Float64Value(l), Float64Value(r)) => Float64Value(l + r)

      case (SUBTRACT, Int8Value(l), Int8Value(r)) => Int8Value((l - r).asInstanceOf[Byte])
      case (SUBTRACT, Int16Value(l), Int16Value(r)) => Int16Value((l - r).asInstanceOf[Short])
      case (SUBTRACT, Int32Value(l), Int32Value(r)) => Int32Value(l - r)
      case (SUBTRACT, Int64Value(l), Int64Value(r)) => Int64Value(l - r)
      case (SUBTRACT, Float32Value(l), Float32Value(r)) => Float32Value(l - r)
      case (SUBTRACT, Float64Value(l), Float64Value(r)) => Float64Value(l - r)

      case (LEFT_SHIFT, Int8Value(l), Int8Value(r)) => Int8Value((l << r).asInstanceOf[Byte])
      case (LEFT_SHIFT, Int16Value(l), Int16Value(r)) => Int16Value((l << r).asInstanceOf[Short])
      case (LEFT_SHIFT, Int32Value(l), Int32Value(r)) => Int32Value(l << r)
      case (LEFT_SHIFT, Int64Value(l), Int64Value(r)) => Int64Value(l << r)

      case (RIGHT_SHIFT_ARITHMETIC, Int8Value(l), Int8Value(r)) => Int8Value((l >> r).asInstanceOf[Byte])
      case (RIGHT_SHIFT_ARITHMETIC, Int16Value(l), Int16Value(r)) => Int16Value((l >> r).asInstanceOf[Short])
      case (RIGHT_SHIFT_ARITHMETIC, Int32Value(l), Int32Value(r)) => Int32Value(l >> r)
      case (RIGHT_SHIFT_ARITHMETIC, Int64Value(l), Int64Value(r)) => Int64Value(l >> r)

      case (RIGHT_SHIFT_LOGICAL, Int8Value(l), Int8Value(r)) => Int8Value((l >>> r).asInstanceOf[Byte])
      case (RIGHT_SHIFT_LOGICAL, Int16Value(l), Int16Value(r)) => Int16Value((l >>> r).asInstanceOf[Short])
      case (RIGHT_SHIFT_LOGICAL, Int32Value(l), Int32Value(r)) => Int32Value(l >>> r)
      case (RIGHT_SHIFT_LOGICAL, Int64Value(l), Int64Value(r)) => Int64Value(l >>> r)

      case (AND, Int8Value(l), Int8Value(r)) => Int8Value((l & r).asInstanceOf[Byte])
      case (AND, Int16Value(l), Int16Value(r)) => Int16Value((l & r).asInstanceOf[Short])
      case (AND, Int32Value(l), Int32Value(r)) => Int32Value(l & r)
      case (AND, Int64Value(l), Int64Value(r)) => Int64Value(l & r)

      case (XOR, Int8Value(l), Int8Value(r)) => Int8Value((l ^ r).asInstanceOf[Byte])
      case (XOR, Int16Value(l), Int16Value(r)) => Int16Value((l ^ r).asInstanceOf[Short])
      case (XOR, Int32Value(l), Int32Value(r)) => Int32Value(l ^ r)
      case (XOR, Int64Value(l), Int64Value(r)) => Int64Value(l ^ r)

      case (OR, Int8Value(l), Int8Value(r)) => Int8Value((l | r).asInstanceOf[Byte])
      case (OR, Int16Value(l), Int16Value(r)) => Int16Value((l | r).asInstanceOf[Short])
      case (OR, Int32Value(l), Int32Value(r)) => Int32Value(l | r)
      case (OR, Int64Value(l), Int64Value(r)) => Int64Value(l | r)

      case _ => throw new RuntimeException
    }
  }

  def evalRelop(op: RelationalOperator, left: Value, right: Value): BooleanValue = {
    import tungsten.interpreter.{NullValue => NV}
    def and(l: BooleanValue, r: BooleanValue): BooleanValue = {
      (l, r) match {
        case (BooleanValue(true), BooleanValue(true)) => BooleanValue(true)
        case _ => BooleanValue(false)
      }
    }
    def or(l: BooleanValue, r: BooleanValue): BooleanValue = {
      (l, r) match {
        case (BooleanValue(false), BooleanValue(false)) => BooleanValue(false)
        case _ => BooleanValue(true)
      }
    }

    (op, left, right) match {
      case (LESS_THAN, Int8Value(l), Int8Value(r)) => BooleanValue(l < r)
      case (LESS_THAN, Int16Value(l), Int16Value(r)) => BooleanValue(l < r)
      case (LESS_THAN, Int32Value(l), Int32Value(r)) => BooleanValue(l < r)
      case (LESS_THAN, Int64Value(l), Int64Value(r)) => BooleanValue(l < r)
      case (LESS_THAN, Float32Value(l), Float32Value(r)) => BooleanValue(l < r)
      case (LESS_THAN, Float64Value(l), Float64Value(r)) => BooleanValue(l < r)
      
      case (LESS_EQUAL, Int8Value(l), Int8Value(r)) => BooleanValue(l <= r)
      case (LESS_EQUAL, Int16Value(l), Int16Value(r)) => BooleanValue(l <= r)
      case (LESS_EQUAL, Int32Value(l), Int32Value(r)) => BooleanValue(l <= r)
      case (LESS_EQUAL, Int64Value(l), Int64Value(r)) => BooleanValue(l <= r)
      case (LESS_EQUAL, Float32Value(l), Float32Value(r)) => BooleanValue(l <= r)
      case (LESS_EQUAL, Float64Value(l), Float64Value(r)) => BooleanValue(l <= r)
      
      case (GREATER_THAN, Int8Value(l), Int8Value(r)) => BooleanValue(l > r)
      case (GREATER_THAN, Int16Value(l), Int16Value(r)) => BooleanValue(l > r)
      case (GREATER_THAN, Int32Value(l), Int32Value(r)) => BooleanValue(l > r)
      case (GREATER_THAN, Int64Value(l), Int64Value(r)) => BooleanValue(l > r)
      case (GREATER_THAN, Float32Value(l), Float32Value(r)) => BooleanValue(l > r)
      case (GREATER_THAN, Float64Value(l), Float64Value(r)) => BooleanValue(l > r)
      
      case (GREATER_EQUAL, Int8Value(l), Int8Value(r)) => BooleanValue(l >= r)
      case (GREATER_EQUAL, Int16Value(l), Int16Value(r)) => BooleanValue(l >= r)
      case (GREATER_EQUAL, Int32Value(l), Int32Value(r)) => BooleanValue(l >= r)
      case (GREATER_EQUAL, Int64Value(l), Int64Value(r)) => BooleanValue(l >= r)
      case (GREATER_EQUAL, Float32Value(l), Float32Value(r)) => BooleanValue(l >= r)
      case (GREATER_EQUAL, Float64Value(l), Float64Value(r)) => BooleanValue(l >= r)

      case (EQUAL, UnitValue, UnitValue) => BooleanValue(true)
      case (EQUAL, BooleanValue(l), BooleanValue(r)) => BooleanValue(l == r)      
      case (EQUAL, Int8Value(l), Int8Value(r)) => BooleanValue(l == r)
      case (EQUAL, Int16Value(l), Int16Value(r)) => BooleanValue(l == r)
      case (EQUAL, Int32Value(l), Int32Value(r)) => BooleanValue(l == r)
      case (EQUAL, Int64Value(l), Int64Value(r)) => BooleanValue(l == r)
      case (EQUAL, Float32Value(l), Float32Value(r)) => BooleanValue(l == r)
      case (EQUAL, Float64Value(l), Float64Value(r)) => BooleanValue(l == r)
      case (EQUAL, NV, NV) => BooleanValue(true)
      case (EQUAL, NV, _: ScalarReferenceValue) => BooleanValue(false)
      case (EQUAL, _: ScalarReferenceValue, NV) => BooleanValue(false)
      case (EQUAL, l: ScalarReferenceValue, r: ScalarReferenceValue) => BooleanValue(l eq r)
      case (EQUAL, l: ArrayValue, r: ArrayValue) => {
        (l.value zip r.value).foldLeft(BooleanValue(true)) { (b, lr) =>
          val (l, r) = lr
          val cmp = evalRelop(EQUAL, l, r)
          and(b, cmp)
        }
      }
      
      case (NOT_EQUAL, UnitValue, UnitValue) => BooleanValue(false)
      case (NOT_EQUAL, BooleanValue(l), BooleanValue(r)) => BooleanValue(l != r)
      case (NOT_EQUAL, Int8Value(l), Int8Value(r)) => BooleanValue(l != r)
      case (NOT_EQUAL, Int16Value(l), Int16Value(r)) => BooleanValue(l != r)
      case (NOT_EQUAL, Int32Value(l), Int32Value(r)) => BooleanValue(l != r)
      case (NOT_EQUAL, Int64Value(l), Int64Value(r)) => BooleanValue(l != r)
      case (NOT_EQUAL, Float32Value(l), Float32Value(r)) => BooleanValue(l != r)
      case (NOT_EQUAL, Float64Value(l), Float64Value(r)) => BooleanValue(l != r)
      case (NOT_EQUAL, NV, NV) => BooleanValue(false)
      case (NOT_EQUAL, NV, _: ScalarReferenceValue) => BooleanValue(true)
      case (NOT_EQUAL, _: ScalarReferenceValue, NV) => BooleanValue(true)
      case (NOT_EQUAL, l: ScalarReferenceValue, r: ScalarReferenceValue) => BooleanValue(l ne r)
      case (NOT_EQUAL, l: ArrayValue, r: ArrayValue) => {
        (l.value zip r.value).foldLeft(BooleanValue(false)) { (b, lr) =>
          val (l, r) = lr
          val cmp = evalRelop(NOT_EQUAL, l, r)
          or(b, cmp)
        }
      }

      case _ => throw new RuntimeException
    }
  }
}
