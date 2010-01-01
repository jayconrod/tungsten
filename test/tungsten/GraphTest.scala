package tungsten

import org.junit.Test
import org.junit.Assert._

class GraphTest {
  @Test
  def initialGraphEmpty = {
    val g = new Graph[Symbol]
    assertTrue(g.nodes.isEmpty)
    assertTrue(g.incident.isEmpty)
    assertTrue(g.adjacent.isEmpty)
  }

  @Test
  def generatedIncident = {
    val g = new Graph(List(1, 2), Map((1, Set(2))))
    assertEquals(Set(1, 2), g.nodes)
    assertEquals(Set(2), g.adjacent(1))
    assertEquals(Set(1), g.incident(2))
  }

  @Test
  def addNode = {
    var g = new Graph[Symbol]
    val sym = Symbol("foo")
    g += sym
    assertEquals(Set(sym), g.nodes)
    assertTrue(g.incident(sym).isEmpty)
    assertTrue(g.adjacent(sym).isEmpty)
  }

  @Test
  def addNodes = {
    val u = Symbol("u")
    val v = Symbol("v")
    val g = new Graph[Symbol] ++ List(u, v)
    assertEquals(Set(u, v), g.nodes)
  }

  @Test
  def addEdge = {
    val u = Symbol("u")
    val v = Symbol("v")
    var g = new Graph[Symbol] ++ List(u, v)
    g &= (u, v)
    assertEquals(Set(v), g.adjacent(u))
    assertEquals(Set(u), g.incident(v))
  }

  @Test
  def addEdges = {
    val u = Symbol("u")
    val v = Symbol("v")
    var g = new Graph[Symbol] ++ List(u, v)
    g &&= List((u, v), (v, u))
    assertEquals(Set(v), g.adjacent(u))
    assertEquals(Set(u), g.adjacent(v))
  }    

  @Test
  def addExistingNodeDoesNotResetEdges = {
    val u = Symbol("u")
    var g = new Graph[Symbol] + u & (u, u)
    g += u
    assertEquals(Set(u), g.adjacent(u))
  }
}

    
