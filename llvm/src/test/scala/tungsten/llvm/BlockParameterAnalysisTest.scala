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

package tungsten.llvm

import tungsten.Graph
import org.junit.Test
import org.junit.Assert._

class BlockParameterAnalysisTest {
  val blocks = List(Block("bb0", List(BitCastInstruction("x", IntValue(0L, 32), IntType(32)),
                                      BranchInstruction(DefinedValue("bb1", LabelType)))),
                    Block("bb1", List(BranchInstruction(DefinedValue("bb2", LabelType)))),
                    Block("bb2", List(ReturnInstruction(DefinedValue("x", IntType(32))))))
  val function = Function("f", Set(), IntType(32), Nil, Set(), blocks)
  val module = new Module(None, None, Map((function.name -> function)))
  val analysis = new BlockParameterAnalysis(module)

  val nodes = blocks.map(new BlockParameterNode(_))
  val cfg = new Graph[BlockParameterNode](nodes) &
    (nodes(0) -> nodes(1)) & (nodes(1) -> nodes(2))

  @Test
  def nodeTest {
    assertEquals(Set("x"), nodes(0).defined)
    assertTrue(nodes(1).defined.isEmpty)
    assertTrue(nodes(2).defined.isEmpty)

    assertTrue(nodes(0).used.isEmpty)
    assertTrue(nodes(1).used.isEmpty)
    assertEquals(Set("x"), nodes(2).used)

    assertTrue(nodes(0).free(Set("x")).isEmpty)
    assertEquals(Set("x"), nodes(1).free(Set("x")))
    assertEquals(Set("x"), nodes(2).free(Set()))
  }

  @Test
  def cfgTest {
    assertEquals(cfg, analysis.cfg(function))
  }

  @Test
  def dataflowTest {
    val result = analysis(cfg)
    assertTrue(result((nodes(0), nodes(1))).contains("x"))
    assertTrue(result((nodes(1), nodes(2))).contains("x"))
  }

  @Test
  def extractTest {
    val result = Map(((nodes(0), nodes(1)) -> Map(("x", "x.1"))),
                     ((nodes(1), nodes(2)) -> Map(("x", "x.2"))))
    val processed = analysis.extract(result, cfg)
    val expected = Map(("bb0" -> BlockParameterData(Nil,
                                                    Map(("bb1" -> List(DefinedValue("x", IntType(32))))))),
                       ("bb1" -> BlockParameterData(List(("x.1", IntType(32))),
                                                    Map(("bb2" -> List(DefinedValue("x.1", IntType(32))))))),
                       ("bb2" -> BlockParameterData(List(("x.2", IntType(32))),
                                                    Map())))
    assertEquals(expected, processed)
  }
}
