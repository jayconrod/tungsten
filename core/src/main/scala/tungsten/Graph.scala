/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

import scala.math.min
import Utilities._

/** This class represents a directed graph and provides several algorithms for their 
 *  manipulation. Nodes in the graph may be of any type that can be used efficiently in a 
 *  set or as key for a map. Since this is an immutable data structure, incident (in) edges,
 *  and adjacent (out) edges are stored separately from nodes to avoid cyclic references.
 *  The incident and adjacent lists are stored as separate maps from nodes to sets of 
 *  neighbors. For the main constructor, every node must have an entry in both lists. The
 *  other constructors compensate for this.
 */
final class Graph[T](val nodes: Set[T],
                     val incident: Map[T, Set[T]],
                     val adjacent: Map[T, Set[T]])
{
  /** Constructs a graph given a set of nodes and an adjacency map. Entries may be missing
   *  from the map (they will be filled in automatically). The incidency map is constructed
   *  from the adjacency map.
   */
  def this(nodes: Traversable[T], adjacent: Map[T, Set[T]]) = {
    this(nodes.toSet,
         (padMap[T, Set[T]](Map(), nodes.toSet, Set()) /: nodes) { (inc, from) =>
           val neighbors = adjacent.getOrElse(from, Set())
           (inc /: neighbors) { (inc, to) =>
             inc + (to -> (inc(to) + from))
           }
         },
         padMap(adjacent, nodes.toSet, Set()))
  }

  /** Constructs a graph given only a set of nodes. No edges are added. */
  def this(nodes: Traversable[T]) = this(nodes, Map())

  /** Constructs an empty graph */
  def this() = this(Set())

  private final def copy(nodes: Set[T] = this.nodes,
                         incident: Map[T, Set[T]] = this.incident,
                         adjacent: Map[T, Set[T]] = this.adjacent) =
  {
    new Graph[T](nodes, incident, adjacent)
  }

  /** Returns copy of this graph with a new node added. If the node is already in this graph,
   *  this graph is returned.
   */
  def + (n: T) = {
    if (nodes.contains(n))
      this
    else
      copy(nodes = nodes + n)
  }

  /** Returns a copy with a collection of nodes added */
  def ++ (ns: Traversable[T]) = ns.foldLeft(this)(_ + _)

  /** Returns a copy of this graph with an edge added. The edge is represented as a pair,
   *  going from the first node to the second. Both nodes must already be present in the graph.
   */
  def & (e: (T, T)) = {
    assert(nodes.contains(e._1) && nodes.contains(e._2))

    copy(incident = incident + (e._2 -> (incident.getOrElse(e._2, Set()) + e._1)),
         adjacent = adjacent + (e._1 -> (adjacent.getOrElse(e._1, Set()) + e._2)))
  }

  /** Returns a copy of this graph with a collection of edges added */
  def && (es: Traversable[(T, T)]) = es.foldLeft(this)(_ & _)

  /** Returns graph with the direction of every edge reversed */
  def reverse: Graph[T] = new Graph(nodes, adjacent, incident)

  /** Performs a depth-first search on this graph from the given starting point and returns
   *  a list of nodes in the order they were visited.
   */
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

  /** Returns a graph of strongly connected components from this graph. An SCC is a subgraph 
   *  in which every node is reachable from every other node. In the resulting graph, all the
   *  nodes in each SCC are consolidated into a single node.
   */
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
