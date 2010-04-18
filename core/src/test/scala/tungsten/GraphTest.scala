package tungsten

import org.junit.Test
import org.junit.Assert._

class GraphTest {
  @Test
  def initialGraphEmpty {
    val g = new Graph[Symbol]
    assertTrue(g.nodes.isEmpty)
    assertTrue(g.incident.isEmpty)
    assertTrue(g.adjacent.isEmpty)
  }

  @Test
  def generatedIncident {
    val g = new Graph(List(1, 2), Map((1, Set(2))))
    assertEquals(Set(1, 2), g.nodes)
    assertEquals(Set(2), g.adjacent(1))
    assertEquals(Set(), g.incident(1))
    assertEquals(Set(1), g.incident(2))
  }

  @Test
  def addNode {
    var g = new Graph[Symbol]
    val sym = Symbol("foo")
    g += sym
    assertEquals(Set(sym), g.nodes)
    assertEquals(Set(), g.incident.getOrElse(sym, Set()))
    assertEquals(Set(), g.adjacent.getOrElse(sym, Set()))
  }

  @Test
  def addNodes {
    val u = Symbol("u")
    val v = Symbol("v")
    val g = new Graph[Symbol] ++ List(u, v)
    assertEquals(Set(u, v), g.nodes)
  }

  @Test
  def addEdge {
    val u = Symbol("u")
    val v = Symbol("v")
    var g = new Graph[Symbol] ++ List(u, v)
    g &= (u, v)
    assertEquals(Set(v), g.adjacent(u))
    assertEquals(Set(u), g.incident(v))
  }

  @Test
  def addEdges {
    val u = Symbol("u")
    val v = Symbol("v")
    var g = new Graph[Symbol] ++ List(u, v)
    g &&= List((u, v), (v, u))
    assertEquals(Set(v), g.adjacent(u))
    assertEquals(Set(u), g.adjacent(v))
  }    

  @Test
  def addExistingNodeDoesNotResetEdges {
    val u = Symbol("u")
    var g = new Graph[Symbol] + u & (u, u)
    g += u
    assertEquals(Set(u), g.adjacent(u))
  }

  @Test
  def reverseTest {
    val inc = Map((2, Set(1)))
    val adj = Map((1, Set(2)))
    val g = new Graph[Int](Set(1, 2), inc, adj)
    val greversed = new Graph[Int](Set(1, 2), adj, inc)
    assertEquals(greversed, g.reverse)
  }                           

  @Test
  def depthFirstTest {
    val g = new Graph[Int](Set(1, 2, 3),
                           Map(1 -> Set(3), 2 -> Set(1), 3 -> Set(1)),
                           Map(1 -> Set(2, 3), 2 -> Set(), 3 -> Set(1)))
    assertEquals(List(1, 2, 3), g.depthFirstList(1))
    assertEquals(List(2), g.depthFirstList(2))
    assertEquals(List(3, 1, 2), g.depthFirstList(3))
  }

  @Test
  def findSCCs {
    val g = new Graph[Int](List(1, 2, 3, 4, 5),
                           Map((1, Set(2)),
                               (2, Set(3)),
                               (3, Set(4)),
                               (4, Set(5)),
                               (5, Set(3))))
    val expected = new Graph[Set[Int]](List(Set(1), Set(2), Set(3, 4, 5)),
                                       Map((Set(1), Set(Set(2))),
                                           (Set(2), Set(Set(3, 4, 5))),
                                           (Set(3, 4, 5), Set(Set(3, 4, 5)))))
    assertEquals(expected, g.findSCCs)
  }
}

    
