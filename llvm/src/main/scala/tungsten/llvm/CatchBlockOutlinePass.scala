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
        case Some((catchBlock, _)) => {
          val catchBlock = block.catchBlock.get
          val predecessors = cfg.incident(block.name)
          val terminatorInst = module.getInstruction(block.instructions.last)
          val terminator = terminatorInst match {
            case _: tungsten.ReturnInstruction => TReturn
            case b: tungsten.BranchInstruction => TBranch(b.target)
            case c: tungsten.ConditionalBranchInstruction => 
              TCondBranch(c.trueTarget, c.falseTarget)
            case _ => TInternal
          }
          val superblock = Superblock(block.name, catchBlock, predecessors, 
                                      Set(block.name), terminator)
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
        val sameCatchBlock = superblock.catchBlock == other.catchBlock
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
      (superblockMap /: mergedSuperblock.blocks) { (m, b) =>
        m + (b -> mergedSuperblock)
      }
    }

    superblock.terminator match {
      case TBranch(targetName) => {
        getTarget(targetName) match {
          case Some(target) if target == superblock => {
            val merged = superblock.copy(terminator = TInternal)
            mergeMap(merged)
          }
          case Some(target) => {
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
    var m = module
    val (tryName, m_1) = createOutlinedFunction(superblock, functionName, module)
    val blockName = symbolFactory(functionName + "tryCall$")
    m = m_1

    def createAllocInsts(targetName: Symbol, prefix: Symbol, suffix: String, module: tungsten.Module) = {
      val block = module.getBlock(targetName)
      val parameterTypes = module.getParameters(block.parameters).map(_.ty)
      parameterTypes map { pty =>
        val name = symbolFactory(blockName + suffix)
        val ty = tungsten.PointerType(pty)
        tungsten.StackAllocateInstruction(name, ty)
      }
    }

    def createLoadAndBranchInsts(slots: List[tungsten.Value],
                                 target: Symbol,
                                 prefix: Symbol,
                                 module: tungsten.Module): List[tungsten.Instruction] =
    {
      val loadInsts = slots map { slot =>
        val name = symbolFactory(prefix + "load$")
        val ty = slot.ty.asInstanceOf[tungsten.PointerType].elementType
        tungsten.LoadInstruction(name, ty, slot)
      }
      val branchInst = tungsten.BranchInstruction(symbolFactory(prefix + "branch$"),
                                                  tungsten.UnitType,
                                                  target,
                                                  loadInsts.map(_.makeValue))
      loadInsts :+ branchInst
    }

    superblock.predecessors foreach { predName =>
      val pred = m.getBlock(predName)
      val predInst = m.getInstruction(pred.instructions.last)
      val newPredInst = predInst match {
        case b: tungsten.BranchInstruction =>
          b.copy(target = blockName)
        case c: tungsten.ConditionalBranchInstruction => {
          var inst = c
          if (c.trueTarget == superblock.head)
            inst = inst.copy(trueTarget = blockName)
          if (c.falseTarget == superblock.head)
            inst = inst.copy(falseTarget = blockName)
          inst
        }
        case _ => throw new RuntimeException("unsupported instruction")
      }
      m = m.replace(newPredInst)
    }

    val head = m.getBlock(superblock.head)
    val (blockParameters, m_2) = createParameters(blockName, superblock.head, m)
    m = m_2

    val callName = symbolFactory(blockName + "call$")
    val branchName = symbolFactory(blockName + "branch$")
    val branchBlockName = symbolFactory(functionName + "tryBranch$")
    val (instructions, extraBlockNames) = superblock.terminator match {
      case TInternal => {
        val callInst = tungsten.StaticCallInstruction(callName, tungsten.UnitType,
                                                      tryName,
                                                      Nil, blockParameters.map(_.makeValue))
        val branchInst = tungsten.BranchInstruction(branchName, tungsten.UnitType,
                                                    branchBlockName, Nil)

        val unreachableInst = tungsten.UnreachableInstruction(symbolFactory(branchBlockName + "unreachable$"),
                                                              tungsten.UnitType)
        val branchBlock = tungsten.Block(branchBlockName, Nil, List(unreachableInst.name))
        m = m.add(unreachableInst).add(branchBlock)

        val insts = List(callInst, branchInst)
        (insts, Nil)
      }
      case TReturn => {
        val returnType = module.getFunction(functionName).returnType
        val callInst = tungsten.StaticCallInstruction(callName, returnType,
                                                      tryName,
                                                      Nil, blockParameters.map(_.makeValue))
        val branchInst = tungsten.BranchInstruction(branchName, tungsten.UnitType,
                                                    branchBlockName,
                                                    List(callInst.makeValue))

        val branchReturnParam = tungsten.Parameter(symbolFactory(branchBlockName + "param$"),
                                                   module.getFunction(functionName).returnType)
        val retInst = tungsten.ReturnInstruction(symbolFactory(branchBlockName + "ret$"),
                                                 tungsten.UnitType,
                                                 branchReturnParam.makeValue)
        val branchBlock = tungsten.Block(branchBlockName,
                                         List(branchReturnParam.name),
                                         List(retInst.name))
        m = m.add(branchReturnParam).add(retInst).add(branchBlock)

        val insts = List(callInst, branchInst)
        (insts, Nil)
      }
      case TBranch(target) => {
        val parametersOutAllocInsts = createAllocInsts(target, blockName, "tryOut$", m)
        val callArguments = blockParameters.map(_.makeValue) ++ 
          parametersOutAllocInsts.map(_.makeValue)
        val callInst = tungsten.StaticCallInstruction(callName, tungsten.UnitType,
                                                      tryName,
                                                      Nil, callArguments)
        val branchInst = tungsten.BranchInstruction(branchName, tungsten.UnitType,
                                                    branchBlockName,
                                                    parametersOutAllocInsts.map(_.makeValue))

        val branchBlockParams = parametersOutAllocInsts.map { p =>
          tungsten.Parameter(symbolFactory(branchBlockName + "param$"), p.ty)
        }
        val parametersOutLoadInsts = createLoadAndBranchInsts(branchBlockParams.map(_.makeValue),
                                                              target, branchBlockName, m)
        val branchBlock = tungsten.Block(branchBlockName,
                                         branchBlockParams.map(_.name),
                                         parametersOutLoadInsts.map(_.name))
        m = m.add(branchBlockParams: _*).add(parametersOutLoadInsts: _*).add(branchBlock)

        val insts = parametersOutAllocInsts ++ List(callInst, branchInst)
        (insts, Nil)
      }
      case TCondBranch(trueTarget, falseTarget) => {
        val trueBlockName = symbolFactory(functionName + "tryTrue$")
        val parametersOutTrueAllocInsts = createAllocInsts(trueTarget, trueBlockName, "tryOutTrue$", m)
        val (trueBranchParameters, m_3) = createPtrParameters(trueBlockName, trueTarget, m)
        m = m_3
        val trueBranchInsts = createLoadAndBranchInsts(trueBranchParameters.map(_.makeValue),
                                                       trueTarget, trueBlockName, m)
        val trueBlock = tungsten.Block(trueBlockName,
                                       trueBranchParameters.map(_.name),
                                       trueBranchInsts.map(_.name))
        m = m.add(trueBranchInsts: _*).add(trueBlock)

        val falseBlockName = symbolFactory(functionName + "tryFalse$")
        val parametersOutFalseAllocInsts = createAllocInsts(falseTarget, falseBlockName, "tryOutFalse$", m)
        val (falseBranchParameters, m_4) = createPtrParameters(falseBlockName, falseTarget, m)
        m = m_4
        val falseBranchInsts = createLoadAndBranchInsts(falseBranchParameters.map(_.makeValue),
                                                        falseTarget, falseBlockName, m)
        val falseBlock = tungsten.Block(falseBlockName,
                                        falseBranchParameters.map(_.name),
                                        falseBranchInsts.map(_.name))
        m = m.add(falseBranchInsts: _*).add(falseBlock)

        val callArguments = blockParameters.map(_.makeValue) ++
          (parametersOutTrueAllocInsts ++ parametersOutFalseAllocInsts).map(_.makeValue)
        val callInst = tungsten.StaticCallInstruction(callName, tungsten.BooleanType,
                                                      tryName,
                                                      Nil, callArguments)
        val branchArguments = (callInst :: 
                               parametersOutTrueAllocInsts ++
                               parametersOutFalseAllocInsts).map(_.makeValue)
        val branchInst = tungsten.BranchInstruction(branchName, tungsten.UnitType,
                                                    branchBlockName,
                                                    branchArguments)
        
        val branchRetParam = tungsten.Parameter(symbolFactory(branchBlockName + "param$"),
                                                tungsten.BooleanType)
        val branchTrueParams = parametersOutTrueAllocInsts map { p =>
          tungsten.Parameter(symbolFactory(branchBlockName + "param$"), p.ty)
        }
        val branchFalseParams = parametersOutFalseAllocInsts map { p =>
          tungsten.Parameter(symbolFactory(branchBlockName + "param$"), p.ty)
        }
        val branchParams = branchRetParam :: branchTrueParams ++ branchFalseParams
        val condInst = tungsten.ConditionalBranchInstruction(symbolFactory(branchBlockName + "cond$"),
                                                             tungsten.UnitType,
                                                             branchRetParam.makeValue,
                                                             trueBlockName,
                                                             branchTrueParams.map(_.makeValue),
                                                             falseBlockName,
                                                             branchFalseParams.map(_.makeValue))
        val branchBlock = tungsten.Block(branchBlockName,
                                         branchParams.map(_.name),
                                         List(condInst.name))
        m = m.add(branchParams: _*).add(condInst).add(branchBlock)

        val insts = parametersOutTrueAllocInsts ++
          parametersOutFalseAllocInsts ++
          List(callInst, branchInst)

        (insts, List(trueBlockName, falseBlockName))
      }
    }

    val origParameterNames = module.getBlock(superblock.head).parameters
    val origCatchBlock = superblock.catchBlock
    val tryBlockParameterNames = blockParameters.map(_.name)
    val catchBlockSubstitution = (origParameterNames zip tryBlockParameterNames).toMap
    val catchBlockArgs = origCatchBlock._2.map(_.substitute(catchBlockSubstitution))
    val catchBlock = (origCatchBlock._1, catchBlockArgs)

    val block = tungsten.Block(blockName,
                               tryBlockParameterNames,
                               instructions.map(_.name),
                               Some(catchBlock))

    val function = m.getFunction(functionName)
    val newBlockNames = function.blocks.filterNot(superblock.blocks.contains _) ++
      (blockName :: branchBlockName :: extraBlockNames)
    val newFunction = function.copy(blocks = newBlockNames)

    m = m.add(instructions: _*).add(block).replace(newFunction)
    m
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
        val (parametersOut, m_3) = createPtrParameters(tryName, target, m)
        m = m_3
        val (epilogueName, m_4) = createEpilogueBlock(tryName, parametersOut, tungsten.UnitValue, m)
        m = m_4

        m = fixBranchInstructions(target, epilogueName, superblock.blocks, m)

        (parametersOut, List(epilogueName))
      }
      case TCondBranch(trueTarget, falseTarget) => {
        val (trueParametersOut, m_5) = createPtrParameters(tryName, trueTarget, m)
        m = m_5
        val (falseParametersOut, m_6) = createPtrParameters(tryName, falseTarget, m)
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

    superblock.blocks.foreach { blockName =>
      val b = m.getBlock(blockName).copy(catchBlock = None)
      m = m.replace(b)
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
    val newParameters = parameters.map(_.copy(name = symbolFactory(tryName + "param$")))
    val m = module.add(newParameters: _*)
    (newParameters, m)
  }

  def createPtrParameters(tryName: Symbol,
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
    val branchInst = tungsten.BranchInstruction(symbolFactory(blockName + "branch$"),
                                                tungsten.UnitType,
                                                entryName,
                                                parameters.map(_.makeValue))
    val block = tungsten.Block(blockName, Nil, List(branchInst.name))
    (blockName, module.add(branchInst).add(block))
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
        case c: tungsten.ConditionalBranchInstruction => {
          val tt = if (c.trueTarget == oldTarget) newTarget else c.trueTarget
          val ft = if (c.falseTarget == oldTarget) newTarget else c.falseTarget
          c.copy(trueTarget = tt, falseTarget = ft)
        }
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
                      catchBlock: (Symbol, List[tungsten.Value]),
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
