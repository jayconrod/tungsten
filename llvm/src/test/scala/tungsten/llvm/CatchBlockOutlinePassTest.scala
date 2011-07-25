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
}
