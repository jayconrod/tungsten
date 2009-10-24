package tungsten.interpreter

import scala.collection.immutable.Stack
import tungsten._
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
      case BranchInstruction(name, bbName, args, _) => {
        val iargs = args.map(Value.eval(_, this))
        val block = module.get[Block](bbName).get
        branch(block, iargs)
        state = state.add(name, UnitValue)
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
      case ReturnInstruction(_, v, _) => {
        val ret = Value.eval(v, this)
        state = stack.head
        stack = stack.pop
        state = state.add(state.inst.name, ret).next
        UnitValue   // gets assigned to the actual return instruction
      }
      case StaticCallInstruction(name, target, arguments, _) => {
        val function = module.get[Function](target).get
        val args = arguments.map(Value.eval(_, this))
        call(function, args)
        UnitValue
      }
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
}
