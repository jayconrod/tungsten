package tungsten.llvm

import tungsten.{DataFlow, Graph, Symbol}
import tungsten.Utilities._

class PhiAnalysis(module: tungsten.Module)
  extends DataFlow
{
  type Node = tungsten.Block

  type Data = Map[Symbol, tungsten.Value]

  def bottom(u: Node, v: Node): Data = {
    val terminator = module.getInstruction(u.instructions.last)
    val arguments = terminator.liveOutBindings(v.name)
    val parameterNames = v.parameters
    assert(arguments.size == parameterNames.size)
    Map((parameterNames zip arguments): _*)
  }

  def argumentsToData(arguments: List[tungsten.Value], block: tungsten.Block): Data = {
    assert(arguments.size == block.parameters.size)
    Map((block.parameters zip arguments): _*)
  }

  def replaceConstants(value: tungsten.Value, 
                       constants: Map[Symbol, tungsten.Value]): tungsten.Value = 
  {
    value match {
      case tungsten.ArrayValue(elementType, elements) => 
        tungsten.ArrayValue(elementType, elements.map(replaceConstants(_, constants)))
      case tungsten.StructValue(structName, fields) =>
        tungsten.StructValue(structName, fields.map(replaceConstants(_, constants)))
      case tungsten.DefinedValue(name, ty) => constants.getOrElse(name, value)
      case _ => value
    }
  }      

  def flow(graph: Graph[Node], node: Node, inData: Map[Node, Data]): Map[Node, Data] = {
    val liveInFromFirst = inData.values.headOption.getOrElse(Map())
    val liveInConstants = liveInFromFirst.filter { kv =>
      val (name, value) = kv
      inData.values.forall { liveIn => liveIn(name) == value }
    }
    val liveOutBindings = module.getInstruction(node.instructions.last).liveOutBindings
    (Map[Node, Data]() /: liveOutBindings) { (outData, kv) =>
      val (blockName, arguments) = kv
      val block = module.getBlock(blockName)
      val updatedArgs = arguments.map { v: tungsten.Value => replaceConstants(v, liveInConstants) }
      val data = argumentsToData(updatedArgs, block)
      outData + (block -> data)
    }
  }
}

object PhiConversion {
  def apply(module: tungsten.Module): tungsten.Module = {
    module
  }

  def cfg(function: tungsten.Function, module: tungsten.Module): Graph[tungsten.Block] = {
    val blocks = module.getBlocks(function.blocks)
    val adjacent = (Map[tungsten.Block, Set[tungsten.Block]]() /: blocks) { (adj, node) =>
      val terminator = module.getInstruction(node.instructions.last)
      val out = terminator.liveOutBindings.keys.toSet.map(module.getBlock _)
      adj + (node -> out)
    }
    new Graph(blocks, adjacent)
  }
}
