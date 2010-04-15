package tungsten

import org.junit.Test
import org.junit.Assert._

class DataFlowTest {
  object BooleanDataFlow extends DataFlow {
    type Node = Int
    type Data = Boolean

    def bottom = false

    def top = true

    def flow(graph: Graph[Int], node: Int, inData: Map[Int, Boolean]): Map[Int, Boolean] = {
      (Map[Int, Boolean]() /: graph.incident(node)) { (out, n) => out + (n -> top) }
    }
  }

  @Test
  def dataFlowTest {
    val graph = new Graph(Set(1, 2, 3, 4),
                          Map(1 -> Set(2, 3), 2 -> Set(4), 3 -> Set(4), 4 -> Set(1)),
                          Map(1 -> Set(4), 2 -> Set(1), 3 -> Set(1), 4 -> Set(2, 3)))
    val expected = Map((1, 2) -> true, (1, 3) -> true, (2, 4) -> true, 
                       (3, 4) -> true, (4, 1) -> true)
    val result = BooleanDataFlow(graph)
    assertEquals(expected, result)
  }  
}
