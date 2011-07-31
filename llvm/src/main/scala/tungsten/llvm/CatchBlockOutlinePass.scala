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

class CatchBlockOutlinePass
  extends Function1[tungsten.Module, tungsten.Module]
{
  import SuperblockTerminator._

  private var symbolFactory: tungsten.SymbolFactory = new tungsten.SymbolFactory

  def apply(module: tungsten.Module): tungsten.Module = {
    symbolFactory = new tungsten.SymbolFactory(module.highestSymbolId + 1)
    val functions = module.definitions.values.collect { case f: tungsten.Function => f }
    var m = module
    for (f <- functions)
      m = outlineFunction(f, m)
    m
  }

  def outlineFunction(function: tungsten.Function, module: tungsten.Module): tungsten.Module = {
    val superblocks = findSuperblocks(function, module)
    var m = module
    for (superblock <- superblocks)
      m = outlineSuperblock(superblock, function.name, m)
    m
  }

  def findSuperblocks(function: tungsten.Function,
                      module: tungsten.Module): Set[Superblock] = 
  {
    System.err.println("findSuperblocks helper called\n")
    val cfg = function.controlFlowGraph(module)
    val entry = module.getBlock(function.blocks.head)
    val (_, superblockMap) = findSuperblocks(entry, Set(), Map(), cfg, module)
    superblockMap.values.toSet
  }

  def findSuperblocks(block: tungsten.Block,
                      visited: Set[Symbol],
                      superblocks: Map[Symbol, Superblock],
                      cfg: Graph[Symbol],
                      module: tungsten.Module): (Set[Symbol], Map[Symbol, Superblock]) =
  {
    if (visited(block.name))
      (visited, superblocks)
    else {
      System.err.println("findSuperblocks(%s)".format(block.name))
      System.err.println("visited: %s".format(visited))

      val successors = cfg.adjacent(block.name).map(module.getBlock _)
      val catchBlocks = block.catchBlock.map { cb => module.getBlock(cb._1) }
      val nextBlocks = successors ++ catchBlocks
      val sVisited = visited + block.name
      val (cVisited, cSuperblocks) = ((sVisited, superblocks) /: nextBlocks) { (i, successor) =>
        val (v, sbs) = i
        findSuperblocks(successor, v, sbs, cfg, module)
      }

      block.catchBlock match {
        case None => (cVisited, cSuperblocks)
        case Some((catchBlockName, _)) => {
          val catchBlockName = block.catchBlock.get._1
          val predecessors = cfg.incident(block.name)
          val terminatorInst = module.getInstruction(block.instructions.last)
          val terminator = terminatorInst match {
            case _: tungsten.ReturnInstruction => TReturn
            case b: tungsten.BranchInstruction => TBranch(b.target)
            case c: tungsten.ConditionalBranchInstruction => 
              TCondBranch(c.trueTarget, c.falseTarget)
            case _ => TInternal
          }
          val superblock = Superblock(block.name, catchBlockName, predecessors, 
                                      Set(block.name), terminator)
          System.err.println("adding %s".format(superblock))
          val simplifiedSuperblocks = simplifySuperblocks(superblock, 
                                                          cSuperblocks + (superblock.head -> superblock))
          (cVisited, simplifiedSuperblocks)
        }
      }
    }
  }

  def simplifySuperblocks(superblock: Superblock,
                          superblockMap: Map[Symbol, Superblock]): Map[Symbol, Superblock] =
  {
    def isCompatible(other: Superblock) = {
      if (other.head == superblock.head)
        true
      else {
        val sameCatchBlock = superblock.catchBlockName == other.catchBlockName 
        val noExternalPredecessors = other.predecessors.forall { pred =>
          superblockMap.get(pred).map(_.head) match {
            case Some(predName) if predName == other.head || predName == superblock.head => true
            case _ => false
          }
        }
        sameCatchBlock && noExternalPredecessors
      }
    }

    def getTarget(targetName: Symbol): Option[Superblock] = {
      superblockMap.get(targetName).filter(isCompatible _)
    }

    def mergeMap(mergedSuperblock: Superblock): Map[Symbol, Superblock] = {
      System.err.println("merged: %s".format(mergedSuperblock))
      (superblockMap /: mergedSuperblock.blocks) { (m, b) =>
        m + (b -> mergedSuperblock)
      }
    }

    System.err.println("simplify(%s)".format(superblock.head))

    superblock.terminator match {
      case TBranch(targetName) => {
        getTarget(targetName) match {
          case Some(target) if target == superblock => {
            System.err.println("infinite loop found")
            val merged = superblock.copy(terminator = TInternal)
            mergeMap(merged)
          }
          case Some(target) => {
            System.err.println("compatible blocks found")
            val merged = superblock.copy(blocks = superblock.blocks ++ target.blocks,
                                         terminator = target.terminator)
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case _ => superblockMap
        }
      }
      case TCondBranch(targetName1, targetName2) if targetName1 == superblock.head => {
        val merged = superblock.copy(terminator = TBranch(targetName2))
        simplifySuperblocks(merged, mergeMap(merged))
      }
      case TCondBranch(targetName1, targetName2) if targetName2 == superblock.head => {
        val merged = superblock.copy(terminator = TBranch(targetName1))
        simplifySuperblocks(merged, mergeMap(merged))
      }
      case TCondBranch(targetName1, targetName2) => {
        val (target1, target2) = (getTarget(targetName1), getTarget(targetName2))
        val (term1, term2) = (target1.map(_.terminator), target2.map(_.terminator))
        System.err.println("targets: %s, %s".format(term1, term2))
        (term1, term2) match {
          case (None, None) if targetName1 == targetName2 => {
            val merged = superblock.copy(terminator = TBranch(targetName1))
            mergeMap(merged)
          }
          case (Some(TInternal), _) => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target1.get.blocks,
                                         terminator = TBranch(targetName2))
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case (_, Some(TInternal)) => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target2.get.blocks,
                                         terminator = TBranch(targetName1))
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case (Some(TReturn), Some(TReturn)) => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target1.get.blocks ++ target2.get.blocks,
                                         terminator = TReturn)
            mergeMap(merged)
          }
          case (Some(TBranch(t)), _) if t == superblock.head => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target1.get.blocks,
                                         terminator = TBranch(targetName2))
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case (_, Some(TBranch(t))) if t == superblock.head => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target2.get.blocks,
                                         terminator = TBranch(targetName1))
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case (Some(TBranch(t1)), _) if t1 == targetName2 => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target1.get.blocks,
                                         terminator = TBranch(targetName2))
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case (_, Some(TBranch(t2))) if t2 == targetName1 => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target2.get.blocks,
                                         terminator = TBranch(targetName1))
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case (Some(TBranch(t1)), Some(TBranch(t2))) if t1 == t2 => {
            val merged = superblock.copy(blocks = superblock.blocks ++ target1.get.blocks ++ target2.get.blocks,
                                         terminator = TBranch(t1))
            simplifySuperblocks(merged, mergeMap(merged))
          }
          case _ => superblockMap
        }
      }
      case _ => superblockMap
    }
  }

  def outlineSuperblock(superblock: Superblock,
                        functionName: Symbol,
                        module: tungsten.Module): tungsten.Module =
  {
    module
  }

  def createOutlinedFunction(superblock: Superblock,
                             functionName: Symbol,
                             module: tungsten.Module): (Symbol, tungsten.Module) =
  {
    val tryName = outlinedFunctionName(functionName)
    var m = module

    val (parametersIn, m_1) = createParameters(tryName, superblock.head, m)
    m = m_1
    val (prologueName, m_2) = createPrologueBlock(tryName, superblock.head, parametersIn, m)
    m = m_2

    val (parametersOut, epilogueBlockNames) = superblock.terminator match {
      case TBranch(target) => {
        val (parametersOut, m_3) = createParameters(tryName, target, m)
        m = m_3
        val (epilogueName, m_4) = createEpilogueBlock(tryName, parametersOut, tungsten.UnitValue, m)
        m = m_4

        m = fixBranchInstructions(target, epilogueName, superblock.blocks, m)

        (parametersOut, List(epilogueName))
      }
      case TCondBranch(trueTarget, falseTarget) => {
        val (trueParametersOut, m_5) = createParameters(tryName, trueTarget, m)
        m = m_5
        val (falseParametersOut, m_6) = createParameters(tryName, falseTarget, m)
        m = m_6
        val (trueEpilogueName, m_7) = createEpilogueBlock(tryName, trueParametersOut,
                                                          tungsten.BooleanValue(true), m)
        m = m_7
        val (falseEpilogueName, m_8) = createEpilogueBlock(tryName, falseParametersOut,
                                                           tungsten.BooleanValue(false), m)
        m = m_8

        m = fixCondBranchInstructions(trueTarget, trueEpilogueName,
                                      falseTarget, falseEpilogueName,
                                      superblock.blocks, m)

        (trueParametersOut ++ falseParametersOut, List(trueEpilogueName, falseEpilogueName))
      }
      case _ => (Nil, Nil)
    }

    val parameterNames = (parametersIn ++ parametersOut).map(_.name)
    val blockNames = prologueName :: superblock.blocks.toList ++ epilogueBlockNames
    val returnType = superblock.terminator match {
      case TReturn => module.getFunction(functionName).returnType
      case TCondBranch(_, _) => tungsten.BooleanType
      case _ => tungsten.UnitType
    }
    val tryFunction = tungsten.Function(tryName, returnType,
                                        Nil, parameterNames,
                                        blockNames)
    m = m.add(tryFunction)
    (tryName, m)
  }

  def createParameters(tryName: Symbol,
                       blockName: Symbol,
                       module: tungsten.Module): (List[tungsten.Parameter], tungsten.Module) =
  {
    val block = module.getBlock(blockName)
    val parameters = module.getParameters(block.parameters)
    val ptrParameters = parameters map { p =>
      val name = symbolFactory(tryName + "param$")
      val ty = tungsten.PointerType(p.ty)
      tungsten.Parameter(name, ty)
    }
    val m = module.add(ptrParameters: _*)
    (ptrParameters, m)
  }

  def createPrologueBlock(tryName: Symbol, 
                          entryName: Symbol,
                          parameters: List[tungsten.Parameter],
                          module: tungsten.Module): (Symbol, tungsten.Module) = 
  {
    val blockName = prologueBlockName(tryName)
    val loadInsts = parameters map { p =>
      val name = symbolFactory(p.name + "load$")
      val ty = p.ty.asInstanceOf[tungsten.PointerType].elementType
      tungsten.LoadInstruction(name, ty, p.makeValue)
    }
    val loadValues = loadInsts.map(_.makeValue)
    val branchInst = tungsten.BranchInstruction(symbolFactory(blockName + "branch$"),
                                                tungsten.UnitType,
                                                entryName,
                                                loadValues)
    val instructions = loadInsts :+ branchInst
    val block = tungsten.Block(blockName, Nil, instructions.map(_.name), None)
    (blockName, module.add(instructions: _*).add(block))
  }

  def createEpilogueBlock(tryName: Symbol,
                          parameters: List[tungsten.Parameter],
                          returnValue: tungsten.Value,
                          module: tungsten.Module): (Symbol, tungsten.Module) =
  {
    val blockName = epilogueBlockName(tryName)
    val blockParameters = parameters map { p =>
      val name = symbolFactory(blockName + "param$")
      val ty = p.ty.asInstanceOf[tungsten.PointerType].elementType
      tungsten.Parameter(name, ty)
    }
    val storeInsts = (parameters zip blockParameters) map { i =>
      val (parameter, blockParameter) = i
      val name = symbolFactory(blockName + "store$")
      tungsten.StoreInstruction(name, tungsten.UnitType,
                                blockParameter.makeValue,
                                parameter.makeValue)
    }
    val returnInst = tungsten.ReturnInstruction(symbolFactory(blockName + "ret$"),
                                                tungsten.UnitType,
                                                returnValue)
    val instructions = storeInsts :+ returnInst
    val block = tungsten.Block(blockName, 
                               blockParameters.map(_.name), 
                               instructions.map(_.name))
    (blockName, module.add(blockParameters: _*).add(instructions: _*).add(block))
  }

  def fixBranchInstructions(oldTarget: Symbol,
                            newTarget: Symbol,
                            blockNames: Set[Symbol],
                            module: tungsten.Module): tungsten.Module =
  {
    (module /: blockNames) { (m, blockName) =>
      val block = m.getBlock(blockName)
      val termInst = m.getInstruction(block.instructions.last)
      val newTermInst = termInst match {
        case b: tungsten.BranchInstruction if b.target == oldTarget =>
          b.copy(target = newTarget)
        case c: tungsten.ConditionalBranchInstruction if c.trueTarget == oldTarget =>
          c.copy(trueTarget = newTarget)
        case c: tungsten.ConditionalBranchInstruction if c.falseTarget == oldTarget =>
          c.copy(falseTarget = newTarget)
        case _ => termInst
      }
      m.replace(newTermInst)
    }
  }

  def fixCondBranchInstructions(oldTrueTarget: Symbol,
                                newTrueTarget: Symbol,
                                oldFalseTarget: Symbol,
                                newFalseTarget: Symbol,
                                blockNames: Set[Symbol],
                                module: tungsten.Module): tungsten.Module =
  {
    (module /: blockNames) { (m, blockName) =>
      val block = m.getBlock(blockName)
      val termInst = m.getInstruction(block.instructions.last)
      val newTermInst = termInst match {
        case c: tungsten.ConditionalBranchInstruction
          if c.trueTarget == oldTrueTarget && c.falseTarget == oldFalseTarget =>
        {
          c.copy(trueTarget = newTrueTarget, falseTarget = newFalseTarget)
        }
        case c: tungsten.ConditionalBranchInstruction
          if c.trueTarget == oldFalseTarget && c.falseTarget == oldTrueTarget =>
        {
          c.copy(trueTarget = newFalseTarget, falseTarget = newTrueTarget)
        }
        case _ => termInst
      }
      m.replace(newTermInst)
    }
  }                                

  def outlinedFunctionName(originalFunctionName: Symbol): Symbol = {
    symbolFactory(originalFunctionName + "try$")
  }

  def prologueBlockName(tryName: Symbol): Symbol = {
    symbolFactory(tryName + "prologue$")
  }

  def epilogueBlockName(tryName: Symbol): Symbol = {
    symbolFactory(tryName + "epilogue$")
  }
}

case class Superblock(head: Symbol,
                      catchBlockName: Symbol,
                      predecessors: Set[Symbol],
                      blocks: Set[Symbol],
                      terminator: SuperblockTerminator)

sealed abstract class SuperblockTerminator {
  def compatibleTerminator(term: SuperblockTerminator): Option[SuperblockTerminator] = {
    import SuperblockTerminator._
    (this, term) match {
      case (l, TInternal) => Some(l)
      case (TInternal, r) => Some(r)
      case (TBranch(lto), TBranch(rto)) if lto == rto => Some(TBranch(lto))
      case (TCondBranch(lto1, lto2), TCondBranch(rto1, rto2))
           if (lto1 == rto1 && lto2 == rto2) || (lto1 == rto2 && lto2 == rto1) =>
        Some(TCondBranch(lto1, lto2))
      case _ => None
    }
  }
}

object SuperblockTerminator {
  case object TInternal extends SuperblockTerminator
  case object TReturn extends SuperblockTerminator
  case class TBranch(to: Symbol) extends SuperblockTerminator
  case class TCondBranch(to1: Symbol, to2: Symbol) extends SuperblockTerminator
}
