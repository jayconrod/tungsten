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
                    "    unit %br = branch @f.exit(int64 %y)\n" +
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
    val (blockName, module) = pass.createPrologueBlock("f.try$", "f.a", parameters, testModule)
    val block = module.getBlock(blockName)
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
  def createEpilogueBlock {
    val parameters = List(tungsten.Parameter("y", tungsten.PointerType(tungsten.IntType(64))))
    val (blockName, module) = pass.createEpilogueBlock("f.try$", parameters, 
                                                       tungsten.BooleanValue(true), testModule)
    val block = module.getBlock(blockName)
    assertSymbolsEqual(List("f.try$.epilogue$.param$"), block.parameters)
    assertSymbolsEqual(List("f.try$.epilogue$.store$", "f.try$.epilogue$.ret$"), block.instructions)
    val storeInst :: retInst :: Nil = module.getInstructions(block.instructions)
    assertEquals(tungsten.StoreInstruction("f.try$.epilogue$.store$#3",
                                           tungsten.UnitType,
                                           tungsten.DefinedValue("f.try$.epilogue$.param$#2",
                                                                 tungsten.IntType(64)),
                                           tungsten.DefinedValue("y",
                                                                 tungsten.PointerType(tungsten.IntType(64)))),
                 storeInst)
    assertEquals(tungsten.ReturnInstruction("f.try$.epilogue$.ret$#4",
                                            tungsten.UnitType,
                                            tungsten.BooleanValue(true)),
                 retInst)
  }

  @Test
  def fixExitBranch {
    val superblock = Superblock("f.a", "f.cb", Set("f.entry"), Set("f.a"), TBranch("f.exit"))
    val module = pass.fixBranchInstructions("f.exit", "epilogue$", superblock.blocks, testModule)
    val termInst = module.getInstruction("f.a.br")
    val expected = tungsten.BranchInstruction(termInst.name,
                                              tungsten.UnitType,
                                              "epilogue$",
                                              List(tungsten.DefinedValue("f.a.y", tungsten.IntType(64))))
    assertEquals(expected, termInst)
  }

  @Test
  def fixExitHalfBranch {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    unit %br = cond true ? @f.b() : @f.exit(int64 12)\n" +
                  "  } catch @f.cb()\n" +
                  "  block %b {\n" +
                  "    branch @f.exit(int64 12)\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb {\n" +
                  "    branch @f.exit(int64 0)\n" +
                  "  }\n" +
                  "  block %exit(int64 %x) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    val superblock = Superblock("f.a", "f.cb", Set("f.entry"), Set("f.a", "f.b"), TBranch("f.exit"))
    val srcModule = linkRuntime(compileString(program))
    val module = pass.fixBranchInstructions("f.exit", "epilogue$", Set("f.a", "f.b"), srcModule)
    val termInst = module.getInstruction("f.a.br")
    val expected = tungsten.ConditionalBranchInstruction(termInst.name,
                                                         tungsten.UnitType,
                                                         tungsten.BooleanValue(true),
                                                         "f.b",
                                                         Nil,
                                                         "epilogue$",
                                                         List(tungsten.IntValue(12, 64)))
    assertEquals(expected, termInst)
  }

  @Test
  def fixExitCondBranch {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    unit %br = cond true ? @f.c(int64 12) : @f.b(int64 34)\n" +
                  "  } catch @f.cb()\n" +
                  "  block %b(int64 %x) {\n" +
                  "    branch @f.exit()\n" +
                  "  }\n" +
                  "  block %c(int64 %x) {\n" +
                  "    branch @f.exit()\n" +
                  "  }\n" +
                  "  block %cb {\n" +
                  "    branch @f.exit()\n" +
                  "  }\n" +
                  "  block %exit {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    val srcModule = linkRuntime(compileString(program))
    val module = pass.fixCondBranchInstructions("f.b", "epilogue$#1",
                                                "f.c", "epilogue$#2",
                                                Set("f.a"), srcModule)
    val termInst = module.getInstruction("f.a.br")
    val expected = tungsten.ConditionalBranchInstruction(termInst.name,
                                                         tungsten.UnitType,
                                                         tungsten.BooleanValue(true),
                                                         "epilogue$#2",
                                                         List(tungsten.IntValue(12, 64)),
                                                         "epilogue$#1",
                                                         List(tungsten.IntValue(34, 64)))
    assertEquals(expected, termInst)
  }

  @Test
  def createOutlinedFunction {
    val superblock = Superblock("f.a", "f.cb", Set("f.entry"), Set("f.a"), TBranch("f.exit"))
    val (tryFunctionName, module) = pass.createOutlinedFunction(superblock, "f", testModule)
    val tryFunction = module.getFunction(tryFunctionName)
    assertSymbolsEqual(List("f.try$.param$", "f.try$.param$"), tryFunction.parameters)
    assertSymbolsEqual(List("f.try$.prologue$", "f.a", "f.try$.epilogue$"), tryFunction.blocks)
  }

  @Test
  def createOutlinedFunctionCondBranch {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    unit %br = cond true ? @f.b(int64 12) : @f.c(int64 34)\n" +
                  "  } catch @f.cb()\n" +
                  "  block %b(int64 %x) {\n" +
                  "    branch @f.exit()\n" +
                  "  }\n" +
                  "  block %c(int64 %x) {\n" +
                  "    branch @f.exit()\n" +
                  "  }\n" +
                  "  block %cb {\n" +
                  "    branch @f.exit()\n" +
                  "  }\n" +
                  "  block %exit {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    val srcModule = linkRuntime(compileString(program))
    val superblock = Superblock("f.a", "f.cb", Set("f.entry"), Set("f.a"), TCondBranch("f.b", "f.c"))
    val (tryFunctionName, module) = pass.createOutlinedFunction(superblock, "f", srcModule)
    val tryFunction = module.getFunction(tryFunctionName)
    assertEquals(2, tryFunction.parameters.size)
    val trueEpilogueName = tryFunction.blocks(tryFunction.blocks.size - 2)
    val trueEpilogue = module.getBlock(trueEpilogueName)
    val trueReturn = module.getInstruction(trueEpilogue.instructions.last).asInstanceOf[tungsten.ReturnInstruction]
    assertEquals(tungsten.ReturnInstruction("ret", tungsten.UnitType, tungsten.BooleanValue(true)),
                 trueReturn.copy(name = "ret"))
    val falseEpilogueName = tryFunction.blocks(tryFunction.blocks.size - 1)
    val falseEpilogue = module.getBlock(falseEpilogueName)
    val falseReturn = module.getInstruction(falseEpilogue.instructions.last).asInstanceOf[tungsten.ReturnInstruction]
    assertEquals(tungsten.ReturnInstruction("ret", tungsten.UnitType, tungsten.BooleanValue(false)),
                 falseReturn.copy(name = "ret"))
  }
}
