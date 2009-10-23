package tungsten.interpreter

import scala.collection.mutable._
import tungsten._
import Value._

final class Environment(val module: Module) {
  val init = List(StaticCallInstruction(new Symbol("init"), new Symbol("main"), Nil),
                  IntrinsicCallInstruction(new Symbol("exit"), 
                                           Intrinsic.EXIT,
                                           List(tungsten.Int32Value(0))))

  val stack = new Stack[State]

  var state: State = new State(init)

  var returnCode = 0

  def run = {
    while (state != null)
      step
    returnCode
  }

  def step = {
    val frame = state
    val inst = frame.ip.head
    val result = eval(inst)
    frame.values += ((inst.name, result))
    frame.ip = frame.ip.tail
  }

  def eval(inst: Instruction): Value = {
    inst match {
      case AssignInstruction(name, value, _) => Value.eval(value, this)
      case BranchInstruction(_, bbName, args, _) => {
        val iargs = args.map(Value.eval(_, this))
        val block = module.get(bbName).get.asInstanceOf[Block]
        branch(block, iargs)
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
        state = stack.pop
        state.values += ((state.ip.head.name, ret))
        state.ip = state.ip.tail
        UnitValue   // gets assigned to the actual return instruction
      }
      case StaticCallInstruction(name, target, arguments, _) => {
        val function = module.get(target).get.asInstanceOf[Function]
        val args = arguments.map(Value.eval(_, this))
        call(function, args)
        UnitValue
      }
    }
  }

  def blockIp(block: Block) = {
    block.instructions.toStream.map(module.get[Instruction](_).get)
  }

  def setArguments(paramNames: List[Symbol], arguments: List[Value]) = {
    assert(paramNames.length == arguments.length)
    for ((name, arg) <- paramNames zip arguments)
      state.values += name -> arg
  }         

  def branch(block: Block, arguments: List[Value]) = {
    state.ip = blockIp(block)
    setArguments(block.parameters, arguments)
  }

  def call(function: Function, arguments: List[Value]) = {
    val block = module.get(function.blocks.head).get.asInstanceOf[Block]
    stack.push(state)
    state = new State(blockIp(block))
    setArguments(function.parameters, arguments)
  }
}
