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
    import SuperblockTerminator._

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
    import SuperblockTerminator._

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
