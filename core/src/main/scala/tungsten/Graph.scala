package tungsten

import scala.math.min
import Utilities._

final class Graph[T](val nodes: Set[T],
                     val incident: Map[T, Set[T]],
                     val adjacent: Map[T, Set[T]])
{
  def this(nodes: Traversable[T], adjacent: Map[T, Set[T]]) = {
    this(nodes.toSet,
         nodes.foldLeft(Map[T, Set[T]]()) { (incident, from) =>
           adjacent.get(from) match {
             case None => incident
             case Some(fromNeighbors) => {
               fromNeighbors.foldLeft(incident) { (incident, to) =>
                 incident + (to -> (incident.getOrElse(to, Set()) + from))
               }
             }
           }
         },
         adjacent)
  }

  def this(nodes: Traversable[T]) = this(nodes, Map[T, Set[T]]())

  def this() = this(Set(), Map(), Map())

  private final def copy(nodes: Set[T] = this.nodes,
                         incident: Map[T, Set[T]] = this.incident,
                         adjacent: Map[T, Set[T]] = this.adjacent) =
  {
    new Graph[T](nodes, incident, adjacent)
  }

  def + (n: T) = {
    if (nodes.contains(n))
      this
    else
      copy(nodes = nodes + n)
  }

  def ++ (ns: Traversable[T]) = ns.foldLeft(this)(_ + _)

  def & (e: (T, T)) = {
    assert(nodes.contains(e._1) && nodes.contains(e._2))

    copy(incident = incident + (e._2 -> (incident.getOrElse(e._2, Set()) + e._1)),
         adjacent = adjacent + (e._1 -> (adjacent.getOrElse(e._1, Set()) + e._2)))
  }

  def && (es: Traversable[(T, T)]) = es.foldLeft(this)(_ & _)

  def reverse: Graph[T] = new Graph(nodes, adjacent, incident)

  def depthFirstList(node: T): List[T] = {
    def depthFirstIter(node: T, list: List[T], visited: Set[T]): (List[T], Set[T]) = {
      if (visited(node))
        (list, visited)
      else {
        val state = (node :: list, visited + node)
        (state /: adjacent(node)) { (state, n) => depthFirstIter(n, state._1, state._2) }
      }
    }
    depthFirstIter(node, Nil, Set[T]())._1.reverse
  }          

  def findSCCs: Graph[Set[T]] = {
    // The following is an implementation of Tarjan's SCC algorithm, adapted
    // from Wikipedia.

    // Overview: We visit the nodes of the graph with a depth first search. The SCCs are the
    // subtrees of the search tree. The root of a subtree is considered the root of the SCC. 
    // So whenever we return from a subtree, we check if the node is a root, then remove 
    // nodes on the stack up until that node if it is. The removed nodes form the SCC. 

    // Root property: We need to be able to check whether a node is a root of an SCC. Each
    // node has an index (in indices). Nodes are numbered in the order they are visited. 
    // Each node also has a lowlink value, which is the minimum index of all reachable nodes. 
    // A node v is a root if and only if indices(v) == lowlink(v). 

    import scala.collection.mutable.Stack
    import scala.collection.mutable.HashMap

    var index = 0
    val stack = new Stack[T]
    val onStack = new HashMap[T, Boolean]       // needed for O(1) stack check
    for (v <- nodes) 
      onStack(v) = false
    val indices = new HashMap[T, Int]
    val lowlink = new HashMap[T, Int]
    var sccs = Set[Set[T]]()

    def tarjan(v: T) {
      indices(v) = index
      lowlink(v) = index
      index += 1
      stack.push(v)
      onStack(v) = true
      for (w <- adjacent.getOrElse(v, Set())) {
        if (!indices.contains(w)) {
          tarjan(w)
          lowlink(v) = min(lowlink(v), lowlink(w))
        } else if (stack.contains(w))
          lowlink(v) = min(lowlink(v), lowlink(w))
      }
      if (lowlink(v) == indices(v)) {
        var scc = Set[T]()
        var w = stack.top // dummy value
        do {
          w = stack.pop
          onStack(w) = false
          scc += w
        } while (v != w)
        sccs += scc
      }
    }

    for (v <- nodes) {
      if (!indices.contains(v))
        tarjan(v)
    }

    // At this point, we have all the SCCs. Now we need to construct a graph.
    val sccMap = new HashMap[T, Set[T]]
    for (scc <- sccs;
         n   <- scc)
      sccMap += n -> scc

    val sccAdjacent = sccs.foldLeft(Map[Set[T], Set[Set[T]]]()) { (adjMap, scc) =>
      val sccAdj = for (u        <- scc;
                        v        <- adjacent.getOrElse(u, Set());
                        val vScc  = sccMap(v))
                     yield vScc
      adjMap + (scc -> sccAdj)
    }
    new Graph(sccs, sccAdjacent)
  }

  override def equals(that: Any) = {
    that match {
      case g: Graph[_] 
        if nodes == g.nodes && incident == g.incident && adjacent == g.adjacent => true
      case _ => false
    }
  }

  override def hashCode = hash("Graph", nodes, adjacent)

  override def toString = {
    val builder = new StringBuilder
    builder.append("Graph {\n")
    for (n <- nodes)
      builder.append("  " + n + " -> " + adjacent(n).mkString(", ") + "\n")
    builder.append("}")
    builder.toString
  }
}
