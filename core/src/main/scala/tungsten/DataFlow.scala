package tungsten

import scala.collection.mutable.Queue

/** Implements an abstraction dataflow fixpoint algorithm. This is the core of most analyses
 *  over different kinds of programs (not necessarily just Tungsten). The analysis is treated
 *  as lattice, which is a tuple (S, R, bottom, top, join, meet). S is a set of constraints. 
 *  R is a relation over P(S). Bottom and top elements of P(S). Bottom should be considered 
 *  the most optimistic solution, while top is the most conservative. Join and meet are 
 *  respectively the least upper bound and greatest lower bound operators with respect to R.
 *
 *  The input to the algorithm is a control flow graph. Forward edges in the graph 
 *  represent forward control flow. The algorithm associates some data with each edge of the
 *  graph. Bottom is used for an initial value for each edge. The algorithm works by applying
 *  a flow function to each node. Given the set of input data for a node, the flow function
 *  produces new output data. The flow function must be monotonic: the new values for the 
 *  output data must be closer to top than the previous ones. This ensures termination,
 *  assuming the lattice has finite height. The algorithm iterates until it reaches the least
 *  fixed point solution.
 */
abstract class DataFlow {
  /** The type of a node in the control flow graph */
  type Node

  /** The type of data we associate with each edge. This is an element from P(S). */
  type Data

  /** The initial data value for each edge. This is the most optimistic (precise) solution.
   *  For all elements a in S, bottom R a holds.
   */
  def bottom: Data

  /** Returns whether the analysis is forward (default) or backward. In a forward analysis,
   *  the input to the flow function is the data on the incident (incoming) edges, and the 
   *  output is new data for the adjacent (outgoing) edges. In a backward analysis,
   *  the opposite is true.
   */
  def isForward: Boolean = true

  /** The flow function operates on data associated with the incident and adjacent edges of 
   *  a node. The result is used to update the edge map (final result). This function must
   *  be monotonic.
   *  @param graph the control flow graph
   *  @param node the node to be operated upon
   *  @param inData a map from incident nodes to data associated with the corresponding edge
   *  @returns a map from adjacent nodes to data associated with the corresponding edge
   */
  def flow(graph: Graph[Node], node: Node, inData: Map[Node, Data]): Map[Node, Data]

  /** The actual dataflow algorithm. It creates a map from edges in the graph to data values
   *  (initially bottom). A worklist is created using the nodes in the graph. It iterates over
   *  the nodes in the worklist until the list is empty. The flow function is called for each
   *  node removed from the worklist. If it returns a new value for any of the adjacent edges,
   *  the corresponding node is added to the worklist.
   *  @param graph the control flow graph to process
   *  @param firstHint a hint indicating the first node to be processed. This is typically
   *    the entry point into the control flow graph.
   *  @returns the least fixed point solution for the graph. This is a map from edges to data.
   */
  final def apply(graph: Graph[Node], 
                  firstHint: Option[Node] = None): Map[(Node, Node), Data] = 
  {
    val edgeMap = new scala.collection.mutable.HashMap[(Node, Node), Data]
    val workList = new scala.collection.mutable.Queue[Node]
    val workSet = new scala.collection.mutable.HashSet[Node]
    workSet ++= graph.nodes

    // Attempt to schedule nodes in depth first order. This will only work if all nodes are
    // reachable from the hinted node.
    firstHint match {
      case Some(hint) => {
        assert(graph.nodes(hint))
        val schedule = graph.depthFirstList(hint)
        if (schedule.size == graph.nodes.size)
          workList ++= schedule
        else
          workList ++= graph.nodes.toList
      }
      case _ => workList ++= graph.nodes.toList
    }

    for (u <- graph.nodes;
         v <- graph.adjacent(u))
      edgeMap += (u, v) -> bottom

    while (!workList.isEmpty) {
      val v = workList.dequeue
      workSet -= v
      val dataIn = if (isForward) {
        (Map[Node, Data]() /: graph.incident(v)) { (data, u) =>
          data + (u -> edgeMap((u, v)))
        }
      } else {
        (Map[Node, Data]() /: graph.adjacent(v)) { (data, u) =>
          data + (u -> edgeMap((v, u)))
        }
      }
      val dataOut = flow(graph, v, dataIn)
      if (isForward) {
        for (w <- graph.adjacent(v)) {
          val oldData = edgeMap((v, w))
          val newData = dataOut(w)
          if (oldData != newData) {
            edgeMap((v, w)) = newData
            if (!workSet(w)) {
              workList.enqueue(w)
              workSet += w
            }
          }
        }
      } else {
        for (w <- graph.incident(v)) {
          val oldData = edgeMap((w, v))
          val newData = dataOut(w)
          if (oldData != newData) {
            edgeMap((w, v)) = newData
            if (!workSet(w)) {
              workList.enqueue(w)
              workSet += w
            }
          }
        }
      }
    }

    (Map[(Node, Node), Data]() /: edgeMap) { (data, kv) => data + kv }
  }
}
