package tungsten.interpreter

import scala.collection.mutable._
import tungsten._
import Value._

final class Environment(val module: Module) {
  val entry = module.get(new Symbol("main")).get.asInstanceOf[Function]
  val stack = new Stack[State]

  var state: State = null

  call(entry, Nil)

  def eval = {
    var inst = state.ip.head
    val result = inst match {
      case BranchInstruction(_, bbName, args, _) => {
        val iargs = args.map(Value.eval(_, this))
        val block = module.get(bbName).get.asInstanceOf[Block]
        branch(block, iargs)
      }
      case IndirectCallInstruction(name, target, arguments, _) => {
        val function = Value.eval(target, this).asInstanceOf[FunctionValue].value
        val args = arguments.map(Value.eval(_, this))
        call(function, args)
      }
      case ReturnInstruction(_, v, _) => {
        state = stack.pop
        val callInst = state.ip.head
        state.values += ((callInst.name, Value.eval(v, this)))
        state.ip = state.ip.tail
      }
      case StaticCallInstruction(name, target, arguments, _) => {
        val function = module.get(target).get.asInstanceOf[Function]
        val args = arguments.map(Value.eval(_, this))
        call(function, args)
      }
    }
  }

  def blockIp(block: Block) = {
    block.instructions.toStream.map(module.get(_).get.asInstanceOf[Instruction])
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
    state = new State(blockIp(block))
    stack.push(state)
    setArguments(function.parameters, arguments)
  }
}
