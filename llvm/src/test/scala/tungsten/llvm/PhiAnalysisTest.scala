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

import org.junit.Test
import org.junit.Assert._
import tungsten.{ModuleIO, Symbol, Graph}
import tungsten.Utilities._

class PhiAnalysisTest {
  val code = "function int64 @f {\n" +
             "  block %bb0 {\n" +
             "    cond true ? @f.bb1(int64 12, int64 34) : @f.bb2(int64 12, int64 56)\n" +
             "  }\n" +
             "  block %bb1(int64 %a, int64 %b) {\n" +
             "    branch @f.bb3(int64 %a, int64 %b)\n" +
             "  }\n" +
             "  block %bb2(int64 %c, int64 %d) {\n" +
             "    branch @f.bb3(int64 %c, int64 %d)\n" +
             "  }\n" +
             "  block %bb3(int64 %e, int64 %f) {\n" +
             "    int64 %x = binop int64 %e + int64 %f\n" +
             "    return int64 %f\n" +
             "  }\n" +
             "}\n"
  val module = ModuleIO.readText(code)
  val function = module.getFunction("f")
  val blockNames = List(0, 1, 2, 3).map { i: Int => symbolFromString("f.bb" + i) }
  val blocks = module.getBlocks(blockNames)
  val graph = function.controlFlowGraphWithCatchBlocks(module)
  val analysis = new PhiAnalysis(module)

  val phiData = Map[(Symbol, Symbol), List[tungsten.Value]]((blockNames(1), blockNames(3)) -> List(tungsten.IntValue(12, 64), tungsten.IntValue(34, 64)),
                                                            (blockNames(2), blockNames(3)) -> List(tungsten.IntValue(12, 64), tungsten.IntValue(56, 64)))
  val argumentMap = Map[Symbol, List[tungsten.Value]](blockNames(1) -> List(tungsten.IntValue(12, 64), tungsten.IntValue(34, 64)),
                                                      blockNames(2) -> List(tungsten.IntValue(12, 64), tungsten.IntValue(56, 64)))
  val phiBindings = List(List[(tungsten.Value, Symbol)]((tungsten.IntValue(12, 64), "f.bb1"), (tungsten.IntValue(12, 64), "f.bb2")),
                         List[(tungsten.Value, Symbol)]((tungsten.IntValue(34, 64), "f.bb1"), (tungsten.IntValue(56, 64), "f.bb2")))
  val constantMap = Map[Symbol, tungsten.Value](symbolFromString("f.bb3.e") -> tungsten.IntValue(12, 64))

  @Test
  def cfg {
    val expected = new Graph[Symbol](blockNames,
                                     Map((blockNames(0), Set(blockNames(1), blockNames(2))),
                                         (blockNames(1), Set(blockNames(3))),
                                         (blockNames(2), Set(blockNames(3))),
                                         (blockNames(3), Set())))
    assertEquals(expected, graph)
  }

  @Test
  def bottom {
    val expected = List(tungsten.IntValue(12, 64), tungsten.IntValue(34, 64))
    val data = analysis.bottom(blockNames(0), blockNames(1))
    assertEquals(expected, data)
  }

  @Test
  def flow {
    val expected = Map(blockNames(3) -> List(tungsten.IntValue(12, 64), tungsten.IntValue(34, 64)))
    val inData = Map(blockNames(0) -> analysis.bottom(blockNames(0), blockNames(1)))
    val outData = analysis.flow(graph, blockNames(1), inData)
    assertEquals(expected, outData)
  }

  @Test
  def isConstant {
    val bindings = List((tungsten.IntValue(12, 64), blockNames(0)),
                        (tungsten.IntValue(12, 64), blockNames(1)))
    assertTrue(PhiConversion.isConstant(bindings))
  }

  @Test
  def isNotConstant {
    val bindings = List((tungsten.IntValue(12, 64), blockNames(0)),
                        (tungsten.IntValue(34, 64), blockNames(1)))
    assertFalse(PhiConversion.isConstant(bindings))
  }

  @Test
  def isConstantEmpty {
    assertFalse(PhiConversion.isConstant(Nil))
  }

  @Test
  def replaceConstants {
    val value = tungsten.DefinedValue("f.bb3.e", tungsten.IntType(64))
    val expected = tungsten.IntValue(12, 64)
    assertEquals(expected, value.mapValues(PhiConversion.replaceConstants(_, constantMap)))
  }

  @Test
  def replaceConstantsArray {
    val value = tungsten.ArrayValue(tungsten.IntType(64), List(tungsten.DefinedValue("f.bb3.e", tungsten.IntType(64))))
    val expected = tungsten.ArrayValue(tungsten.IntType(64), List(tungsten.IntValue(12, 64)))
    assertEquals(expected, value.mapValues(PhiConversion.replaceConstants(_, constantMap)))
  }

  @Test
  def replaceConstantsStruct {
    val value = tungsten.StructValue("A", List(tungsten.DefinedValue("f.bb3.e", tungsten.IntType(64))))
    val expected = tungsten.StructValue("A", List(tungsten.IntValue(12, 64)))
    assertEquals(expected, value.mapValues(PhiConversion.replaceConstants(_, constantMap)))
  }

  @Test
  def argumentMapFromData {
    assertEquals(argumentMap, PhiConversion.argumentMapFromData("f.bb3", graph, phiData))
  }

  @Test
  def phiBindingsFromArgumentMap {
    assertEquals(phiBindings, PhiConversion.phiBindingsFromArgumentMap(argumentMap))
  }

  @Test
  def constantMapFromPhiBindings {
    assertEquals(constantMap, PhiConversion.constantMapFromPhiBindings(blocks(3).parameters, phiBindings))
  }

  @Test
  def rewrite {
    val oldInstructions = module.getInstructions(blocks(3).instructions)
    val expectedInstructions = List(TungstenPhiInstruction("f.bb3.f", tungsten.IntType(64), phiBindings(1)),
                                    tungsten.BinaryOperatorInstruction("f.bb3.x", tungsten.IntType(64), tungsten.BinaryOperator.ADD, tungsten.IntValue(12, 64), tungsten.DefinedValue("f.bb3.f", tungsten.IntType(64))),
                                    oldInstructions(1))
    val expectedBlock = tungsten.Block("f.bb3", Nil, expectedInstructions.map(_.name))
    val expectedModule = module.remove("f.bb3.e").replace((expectedBlock :: expectedInstructions): _*)
    assertEquals(expectedModule, PhiConversion.rewrite(blocks(3), phiBindings, constantMap, module))
  }                                 
}
