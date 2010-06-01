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
      val updatedArgs = arguments.map { v: tungsten.Value => 
        PhiConversion.replaceConstants(v, liveInConstants) 
      }
      val data = argumentsToData(updatedArgs, block)
      outData + (block -> data)
    }
  }
}

object PhiConversion {
  def apply(module: tungsten.Module): tungsten.Module = {
    val functions = module.definitions.valuesIterator.collect { case f: tungsten.Function => f }
    (module /: functions) { (module, function) =>
      val blocks = module.getBlocks(function.blocks)
      val graph = cfg(function, module)
      val analysis = new PhiAnalysis(module)
      val phiData = analysis(graph, blocks.headOption)
      (module /: blocks) { (module, block) => rewrite(block, graph, phiData, module) }
    }
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

  def rewrite(block: tungsten.Block, 
              graph: Graph[tungsten.Block],
              phiData: Map[(tungsten.Block, tungsten.Block), Map[Symbol, tungsten.Value]],
              module: tungsten.Module): tungsten.Module =
  {
    def isConstant(bindings: List[(tungsten.Value, Symbol)]): Boolean = {
      bindings match {
        case Nil => false
        case h :: t => t.forall(_ == h)
      }
    }

    val predecessors = graph.adjacent(block).toList
    val phiBindings = block.parameters.map { parameterName =>
      val bindings = predecessors.map { predecessor =>
        val argumentMap = phiData((predecessor, block))
        (argumentMap(parameterName), predecessor.name)
      }
      (parameterName, bindings)
    }
    val phiNodes = phiBindings.collect { case (name, bindings) if !isConstant(bindings) =>
      val ty = module.getParameter(name).ty
      TungstenPhiInstruction(name, ty, bindings)
    }
    val constants = (Map[Symbol, tungsten.Value]() /: phiBindings) { (constants, phi) =>
      val (parameterName, bindings) = phi
      if (isConstant(bindings))
        constants + (parameterName -> bindings.head._1)
      else
        constants
    }      
    
    val instructions = module.getInstructions(block.instructions)
    val rewrittenInstructions = phiNodes ++ instructions.map { instruction =>
      val rewritten = instruction.mapValues(replaceConstants(_, constants))
      rewritten match {
        case branch: tungsten.BranchInstruction => branch.copyWith("arguments" -> Nil)
        case cond: tungsten.ConditionalBranchInstruction =>
          cond.copyWith("trueArguments" -> Nil, "falseArguments" -> Nil)
        case _ => rewritten
      }
    }
    val rewrittenBlock = block.copyWith("parameters" -> Nil, 
                                        "instructions" -> rewrittenInstructions.map(_.name))
    module.remove(block.parameters).replace(rewrittenBlock :: rewrittenInstructions)
  }
}
