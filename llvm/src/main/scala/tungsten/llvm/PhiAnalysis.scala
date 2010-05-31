package tungsten.llvm

import tungsten.{DataFlow, Graph, Symbol}
import tungsten.Utilities._

class PhiAnalysis(module: tungsten.Module)
  extends DataFlow
{
  type Node = tungsten.Block

  type Data = Map[Symbol, tungsten.Value]

  def bottom(u: Node, v: Node): Data = {
    throw new UnsupportedOperationException
    // TODO: implement
  }

  def flow(graph: Graph[Node], node: Node, inData: Map[Node, Data]): Map[Node, Data] = {
    throw new UnsupportedOperationException
    // TODO: implement
  }
}

object PhiConversion {
  def apply(module: tungsten.Module): tungsten.Module = {
    module
  }

  def cfg(function: tungsten.Function, module: tungsten.Module): Graph[tungsten.Block] = {
    throw new UnsupportedOperationException
    // TODO: implement
  }
}
