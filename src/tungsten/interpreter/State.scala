package tungsten.interpreter

import tungsten.{Instruction, Symbol, Block, Function, Module}

final class State(val blockName: Symbol,
                  val block: Array[Instruction], 
                  val index: Int,
                  val values: Map[Symbol, Value])
{
  def this(block: Block, module: Module) = {
    this(block.name,
         block.instructions.map(module.get[Instruction](_).get).toArray,
         0,
         Map[Symbol, Value]())
  }

  def this(function: Function, module: Module) = {
    this(function.blocks.head,
         module.get[Block](function.blocks.head).get.
           instructions.map(module.get[Instruction](_).get).toArray,
         0,
         Map[Symbol, Value]())
  }    

  def copy(blockName: Symbol = blockName,
           block: Array[Instruction] = block,
           index: Int = index,
           values: Map[Symbol, Value] = values) = new State(blockName, block, index, values)

  def add(value: Value): State = add(block(index).name, value)

  def add(key: Symbol, value: Value): State = copy(values = values + (key -> value))

  def get(key: Symbol) = values.get(key).get

  def inst = block(index)

  def next = copy(index = index + 1)

  override def toString = {
    "State {\n" +
    "  blockName = " + blockName + "\n" +
    "  inst = " + inst.name + " (" + inst + ")\n" +
    "  values = " + 
    values.mkString("{\n    ", "\n    ", "\n  }\n") +
    "}"
  }
}
