package tungsten

import Math.min

final class Graph[T](val nodes: Set[T],
                     val incident: Map[T, Set[T]],
                     val adjacent: Map[T, Set[T]])
{
  def this() = this(Set[T](), Map[T, Set[T]](), Map[T, Set[T]]())

  def this(nodes: Traversable[T]) = this(nodes.toSet, Map[T, Set[T]](), Map[T, Set[T]]())

  def this(nodes: Traversable[T], adjacent: Map[T, Set[T]]) = {
    this(nodes.toSet,
         adjacent.foldLeft(Map[T, Set[T]]()) { (incident, adjPair) =>
           val (from, adj) = adjPair
           adj.foldLeft(incident) { (incident, to) =>
             incident + (to -> (incident.getOrElse(to, Set[T]()) + from))
           }
         },
         adjacent)
  }

  private final def copy(nodes: Set[T] = this.nodes,
                         incident: Map[T, Set[T]] = this.incident,
                         adjacent: Map[T, Set[T]] = this.adjacent) =
  {
    new Graph[T](nodes, incident, adjacent)
  }

  def + (n: T) = {
    if (nodes.contains(n))
      this
    else {
      new Graph(nodes + n,
                incident + (n -> Set[T]()),
                adjacent + (n -> Set[T]()))
    }
  }

  def ++ (ns: Traversable[T]) = ns.foldLeft(this)(_ + _)

  def & (e: (T, T)) = {
    assert(nodes.contains(e._1) && nodes.contains(e._2))

    copy(incident = incident.updated(e._2, incident(e._2) + e._1),
         adjacent = adjacent.updated(e._1, adjacent(e._1) + e._2))
  }

  def && (es: Traversable[(T, T)]) = es.foldLeft(this)(_ & _)

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
      for (w <- adjacent(v)) {
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
                        v        <- adjacent(u);
                        val vScc  = sccMap(v);
                        if vScc ne scc)
                     yield vScc
      adjMap + (scc -> sccAdj)
    }
    new Graph(sccs, sccAdjacent)
  }
}
