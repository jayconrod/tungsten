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

import tungsten.Symbol
import tungsten.Graph
import tungsten.Utilities._
import SuperblockTerminator._
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

class CatchBlockOutlinePassTest {
  val pass = new CatchBlockOutlinePass

  val testProgram = "function unit @f {\n" +
                    "  block %entry {\n" +
                    "    branch @f.a(int64 12)\n" +
                    "  }\n" +
                    "  block %a(int64 %x) {\n" +
                    "    int64 %y = binop int64 %x + int64 34\n" +
                    "    branch @f.exit(int64 %y)\n" +
                    "  } catch @f.cb()\n" +
                    "  block %exit(int64 %y) {\n" +
                    "    return ()\n" +
                    "  }\n" +
                    "  block %cb(class @tungsten.Exception %exn) {\n" +
                    "    return ()\n" +
                    "  }\n" +
                    "}\n"
  lazy val testModule = linkRuntime(compileString(testProgram))

  def makeFunction(blockCode: String): (tungsten.Function, tungsten.Module) = {
    val program = "function unit @f {\n" +
                  blockCode +
                  "  block %cb(class @tungsten.Exception %exn) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "  block %exit {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    val module = linkRuntime(compileString(program))
    val function = module.getFunction("f")
    (function, module)
  }

  def assertSymbolsEqual(expected: Traversable[Symbol], actual: Traversable[Symbol]) {
    assertEquals(expected.map(_.copy(id = 0)), actual.map(_.copy(id = 0)))
  }

  def testFindSuperblock(code: String,
                         headName: Symbol, 
                         blockNames: Set[Symbol],
                         terminator: SuperblockTerminator) 
  {
    val (function, module) = makeFunction(code)
    val cfg = function.controlFlowGraph(module)
    val head = module.getBlock(headName)
    val catchBlockName = head.catchBlock.get._1
    val predecessors = cfg.incident(headName)
    val expected = Superblock(headName, catchBlockName, predecessors, blockNames, terminator)
    val (_, superblockMap) = pass.findSuperblocks(head, Set(), Map(), cfg, module)
    val superblock = superblockMap(headName)
    assertEquals(expected, superblock)
  }

  @Test
  def findDoubleBlock {
    val code = "block %entry {\n" +
               "  branch @f.a()\n" +
               "} catch @f.cb()\n" +
               "block %a {\n" +
               "  branch @f.exit()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.entry", Set("f.entry", "f.a"), TBranch("f.exit"))
  }

  @Test
  def findSelfLoop {
    val code = "block %entry {\n" +
               "  branch @f.a()\n" +
               "}\n" +
               "block %a {\n" +
               "  branch @f.a()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.a", Set("f.a"), TInternal)
  }

  @Test
  def findCondBranchWithLoop {
    val code = "block %entry {\n" +
               "  cond true ? @f.a() : @f.b()\n" +
               "}\n" +
               "block %a {\n" +
               "  branch @f.c()\n" +
               "}\n" +
               "block %b {\n" +
               "  branch @f.b()\n" +
               "}\n" +
               "block %c {\n" +
               "  branch @f.exit()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.c", Set("f.c"), TBranch("f.exit"))
  }

  @Test
  def findNormalLoop {
    val code = "block %entry {\n" +
               "  branch @f.a()\n" +
               "}\n" +
               "block %a {\n" +
               "  cond true ? @f.a() : @f.exit()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.a", Set("f.a"), TBranch("f.exit"))
  }

  @Test
  def findComplexLoop {
    val code = "block %entry {\n" +
               "  branch @f.a()\n" +
               "} catch @f.cb()\n" +
               "block %a {\n" +
               "  branch @f.b()\n" +
               "} catch @f.cb()\n" +
               "block %b {\n" +
               "  cond true ? @f.a() : @f.exit()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.entry", Set("f.entry", "f.a", "f.b"), TBranch("f.exit"))
  }

  @Test
  def findHalfConditional {
    val code = "block %entry {\n" +
               "  cond true ? @f.a() : @f.exit()\n" +
               "} catch @f.cb()\n" +
               "block %a {\n" +
               "  branch @f.exit()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.entry", Set("f.entry", "f.a"), TBranch("f.exit"))
  }

  @Test
  def findDiamond {
    val code = "block %entry {\n" +
               "  cond true ? @f.a() : @f.b()\n" +
               "} catch @f.cb()\n" +
               "block %a {\n" +
               "  branch @f.c()\n" +
               "} catch @f.cb()\n" +
               "block %b {\n" +
               "  branch @f.c()\n" +
               "} catch @f.cb()\n" +
               "block %c {\n" +
               "  branch @f.exit()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.entry", Set("f.entry", "f.a", "f.b", "f.c"), TBranch("f.exit"))
  }

  @Test
  def multiReturn {
    val code = "block %entry {\n" +
               "  cond true ? @f.a() : @f.b()\n" +
               "} catch @f.cb()\n" +
               "block %a {\n" +
               "  return ()\n" +
               "} catch @f.cb()\n" +
               "block %b {\n" +
               "  branch @f.c()\n" +
               "} catch @f.cb()\n" +
               "block %c {\n" +
               "  return ()\n" +
               "} catch @f.cb()\n"
    testFindSuperblock(code, "f.entry", Set("f.entry", "f.a", "f.b", "f.c"), TReturn)
  }

  @Test
  def superblockBehindCatchBlock {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "}\n"
    val module = linkRuntime(compileString(program))
    val function = module.getFunction("f")
    val superblocks = pass.findSuperblocks(function, module)
    val expected = Superblock("f.a", "f.cb", Set("f.cb"), Set("f.a"), TReturn)
    assertTrue(superblocks.contains(expected))
  }

  @Test
  def createPrologueBlock {
    val parameters = List(tungsten.Parameter("x", tungsten.PointerType(tungsten.IntType(64))))
    val module = pass.createPrologueBlock("f", "f.a", parameters, testModule)
    val block = module.getBlock("f.try$.prologue$#1")
    assertEquals(Nil, block.parameters)
    assertSymbolsEqual(List("x.load$", "f.try$.prologue$.branch$"),
                       block.instructions)
    val loadInst :: branchInst :: Nil = module.getInstructions(block.instructions)
    assertEquals(tungsten.LoadInstruction("x.load$#2",
                                          tungsten.IntType(64),
                                          tungsten.DefinedValue("x", tungsten.PointerType(tungsten.IntType(64)))),
                 loadInst)
    assertEquals(tungsten.BranchInstruction("f.try$.prologue$.branch$#3", tungsten.UnitType,
                                            "f.a",
                                            List(tungsten.DefinedValue("x.load$#2",
                                                                       tungsten.IntType(64)))),
                 branchInst)
  }

  @Test
  @Ignore
  def createEpilogueBlockBranch {
  }

  @Test
  @Ignore
  def createEpilogueBlockCondBranch {
  }

  @Test
  @Ignore
  def createEpilogueBlockReturn {
  }

  @Test
  @Ignore
  def createEpilogueInternal {
  }

  @Test
  def createOutlinedFunction {
    val superblock = Superblock("f.a", "f.cb", Set("f.entry"), Set("f.a"), TBranch("f.exit"))
    val module = pass.createOutlinedFunction(superblock, "f", testModule)
    val tryFunction = module.getFunction("f.try$#1")
    assertSymbolsEqual(List("f.try$.param$"), tryFunction.parameters)
    assertSymbolsEqual(List("f.try$.prologue$", "f.a"), tryFunction.blocks)
  }
}
