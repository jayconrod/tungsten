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

import org.junit.Test
import org.junit.Assert._

class DataFlowTest {
  class BooleanDataFlow(override val isForward: Boolean) extends DataFlow {
    type Node = Int
    type Data = Boolean

    def bottom(u: Node, v: Node) = false

    def flow(graph: Graph[Int], node: Int, inData: Map[Int, Boolean]): Map[Int, Boolean] = {
      val outEdges = if (isForward) graph.adjacent else graph.incident
      (Map[Int, Boolean]() /: outEdges(node)) { (out, n) => out + (n -> true) }
    }
  }

  @Test
  def dataFlowTest {
    val analysis = new BooleanDataFlow(true)
    val graph = new Graph(Set(1, 2, 3, 4),
                          Map(1 -> Set(4), 2 -> Set(1), 3 -> Set(1), 4 -> Set(2, 3)),
                          Map(1 -> Set(2, 3), 2 -> Set(4), 3 -> Set(4), 4 -> Set(1)))
    val expected = Map((1, 2) -> true, (1, 3) -> true, (2, 4) -> true, 
                       (3, 4) -> true, (4, 1) -> true)
    val result = analysis(graph)
    assertEquals(expected, result)
  }

  @Test
  def reverseTest {
    val analysis = new BooleanDataFlow(false)
    val graph = new Graph(Set(1, 2, 3, 4)) & 1->2 & 1->3 & 2->4 & 3->4 & 4->1
    val expected = Map((1, 2) -> true, (1, 3) -> true, (2, 4) -> true, 
                       (3, 4) -> true, (4, 1) -> true)
    val result = analysis(graph)
    assertEquals(expected, result)
  }
}
