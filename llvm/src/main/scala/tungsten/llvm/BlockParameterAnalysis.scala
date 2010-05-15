package tungsten.llvm

import tungsten.DataFlow
import tungsten.Graph
import tungsten.Utilities._

case class BlockParameterData(parameters: List[(String, Type)],
                              arguments: Map[String, List[Value]])

class BlockParameterNode(val block: Block) {
  val name = block.name

  val defined = block.instructions.map(_.name).filterNot(_.isEmpty).toSet
  val used = block.instructions.flatMap(_.usedVars).toSet
  var parameters = Map[String, String]()
  def free(liveOut: Set[String]): Set[String] = used ++ liveOut -- defined -- parameters.keys

  override def equals(that: Any): Boolean = {
    that match {
      case n: BlockParameterNode => name == n.name
      case _ => false
    }
  }

  override def hashCode = name.hashCode

  override def toString = "BlockParameterNode(" + name + ")"

}

final class BlockParameterAnalysis(module: Module) extends DataFlow {
  type Node = BlockParameterNode

  /** Key is original free variable, value is parameter name */
  type Data = Map[String, String]

  val bottom = Map[String, String]()

  override def isForward = false

  def cfg(function: Function): Graph[Node] = {
    val nodeMap = (Map[String, Node]() /: function.blocks) { (nodes, block) =>
      nodes + (block.name -> new Node(block))
    }
    (new Graph(nodeMap.values) /: function.blocks) { (g, block) =>
      block.instructions.last match {
        case BranchInstruction(DefinedValue(label, LabelType)) if nodeMap.contains(label) => {
          val node = nodeMap(block.name)
          val succ = nodeMap(label)
          g & (node -> succ)
        }
        case _ => g
      }
    }
  }

  override def flow(graph: Graph[Node], 
                    node: Node, 
                    inData: Map[Node, Data]): Map[Node, Data] = 
  {
    val liveOut = inData.values.flatMap(_.keys).toSet
    for (v <- node.free(liveOut)) {
      node.parameters += v -> freshName(v)
    }
    (Map[Node, Data]() /: graph.incident(node)) { (result, in) => 
      result + (in -> node.parameters)
    }
  }

  def extract(result: Map[(Node, Node), Data],
              graph: Graph[Node]): Map[String, BlockParameterData] =
  {
    val blocks = graph.nodes.map(_.block)
    val types = (Map[String, Type]() /: blocks) { (types, block) =>
      (types /: block.instructions) { (types, inst) =>
        if (inst.name.isEmpty)
          types
        else
          types + (inst.name -> inst.ty(module))
      }
    }

    val emptyRenameMap = (Map[(String, String), String]() /: blocks) { (rename, block) =>
      (rename /: block.instructions) { (rename, inst) =>
        if (inst.name.isEmpty)
          rename
        else
          rename + ((block.name, inst.name) -> inst.name)
      }
    }
    val renameMap = (emptyRenameMap /: result) { (data, kv) =>
      val ((_, succ), bindings) = kv
      (data /: bindings) { (data, kv) =>
        val (origName, paramName) = kv
        data + ((succ.name, origName) -> paramName)
      }
    }

    val emptyParameterMap = padMap[String, List[(String, Type)]](Map(), blocks.map(_.name), Nil)
    val parameterMap = (emptyParameterMap /: result) { (data, kv) =>
      val ((node, succ), bindings) = kv
      val parameters = for ((origName, paramName) <- bindings.toList)
        yield (paramName, types(origName))
      if (data(succ.name).isEmpty && !parameters.isEmpty)
        data + (succ.name -> parameters)
      else {
        assert(data(succ.name) == parameters)
        data
      }
    }
    // TODO: include PHI nodes in parameter lists

    val emptyArgumentMap = padMap[String, Map[String, List[Value]]](Map(), blocks.map(_.name), Map())
    val argumentMap = (emptyArgumentMap /: result) { (data, kv) =>
      val ((node, succ), bindings) = kv
      val arguments = bindings.toList.map(_._1).map { origName =>
        val localName = renameMap((node.name, origName))
        val ty = types(origName)
        DefinedValue(localName, ty)
      }
      data + (node.name -> (data(node.name) + (succ.name -> arguments)))
    }
    // TODO: include PHI nodes in argument lists

    (Map[String, BlockParameterData]() /: blocks.map(_.name)) { (data, name) =>
      data + (name -> BlockParameterData(parameterMap(name), argumentMap(name)))
    }
  }

  def apply(function: Function): Map[String, BlockParameterData] = {
    val graph = cfg(function)
    val result = apply(graph)
    extract(result, graph)
  }

  private var nameCount = 0
  def freshName(origName: String) = {
    nameCount += 1
    origName + "." + nameCount
  }
}

object BlockParameterAnalysis {
  def apply(function: Function, module: Module): Map[String, BlockParameterData] = {
    val analysis = new BlockParameterAnalysis(module)
    val cfg = analysis.cfg(function)
    val data = analysis(cfg)
    analysis.extract(data, cfg)
  }
}
