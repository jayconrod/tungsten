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

  /** The most pessimistic (conservative) solution. For all elements a in S, a R top holds. */
  def top: Data

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
   *  @returns the least fixed point solution for the graph. This is a map from edges to data.
   */
  final def apply(graph: Graph[Node]): Map[(Node, Node), Data] = {
    var edgeMap = Map[(Node, Node), Data]()
    val workList = new Queue[Node]

    System.err.println("Initializing")
    for (u <- graph.nodes) {
      workList.enqueue(u)
      System.err.println("pushed " + u)
      for (v <- graph.adjacent(u)) {
        System.err.println("set " + u + " -> " + v + " to " + bottom)
        edgeMap += (u, v) -> bottom
      }
    }

    while (!workList.isEmpty) {
      val v = workList.dequeue
      System.err.println("dequeued " + v)
      val dataIn = (Map[Node, Data]() /: graph.incident(v)) { (data, u) =>
        data + (u -> edgeMap((u, v)))
      }
      System.err.println("data in: " + dataIn)
      val dataOut = flow(graph, v, dataIn)
      System.err.println("data out: " + dataOut)
      for (w <- graph.adjacent(v)) {
        System.err.println("updating " + w)
        val oldData = edgeMap((v, w))
        System.err.println("  old: " + oldData)
        val newData = dataOut(w)
        System.err.println("  new: " + newData)
        if (oldData != newData) {
          System.err.println("  change detected! enqueueing " + w)
          edgeMap += (v, w) -> newData
          workList.enqueue(w)
        }
      }
      System.err.println("work list is now: " + workList)
    }

    edgeMap
  }
}
