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

/* The purpose of the PhiConversion pass is to convert the live-in/live-out notation
 * from Tungsten style to LLVM style. In Tungsten, live-in variables are described by
 * parameters for each block. Live-out variables are described by explicit arguments
 * for each branch. LLVM uses the more standard Phi instructions. If a block receives
 * two (or more) different values for the same variable from different predecessors,
 * a Phi instruction is written showing which value is received from each block.
 *
 * PhiAnalysis is a data flow analysis which determines which values actually require
 * Phi instructions. PhiConversion actually inserts the Phi instructions and removes
 * block parameters and arguments.
 */

package tungsten.llvm

import tungsten.{DataFlow, Graph, Symbol}
import tungsten.Utilities._

class PhiAnalysis(module: tungsten.Module)
  extends DataFlow
{
  /** Each node is a block name in the CFG for a function */
  type Node = Symbol

  /** The data on each edge is the argument list passed along that branch */
  type Data = List[tungsten.Value]

  /** The initial data set for each branch is the argument list from the original program */
  def bottom(u: Node, v: Node): Data = {
    val predecessor = module.getBlock(u)
    predecessor.liveOutBindings(module)(v)
  }

  /** This function looks at the argument lists incoming from all predecessors to a block. If
   *  a parameter is constant (has the same argument from every predecessor), references to 
   *  it in outgoing argument lists are replaced by the constant value.
   */
  def flow(graph: Graph[Node], node: Node, inData: Map[Node, Data]): Map[Node, Data] = {
    val block = module.getBlock(node)

    /* First, we get a list of PHI bindings. These are in the same format as for a PHI 
     * instruction. We have a set of bindings for each parameter.
     */
    val phiBindings = PhiConversion.phiBindingsFromArgumentMap(inData)

    /* Next, we determine which parameters are constant. A parameter is constant if the 
     * all the corresponding arguments from the predecessors are equal. The map contains
     * an entry for each constant parameter, mapping the parameter name to the constant value.
     */
    val liveInConstants = PhiConversion.constantMapFromPhiBindings(block.parameters, phiBindings)

    /* Finally, we generate the output by updating the arguments to the successors. Any 
     * reference to a constant parameter is replaced by the constant value.
     */
    val liveOutBindings = block.liveOutBindings(module)
    (Map[Node, Data]() /: liveOutBindings) { (outData, kv) =>
      val (blockName, arguments) = kv
      val updatedArgs = arguments.map { v =>
        v.mapValues(PhiConversion.replaceConstants(_, liveInConstants))
      }
      outData + (blockName -> updatedArgs)
    }
  }
}

object PhiConversion
  extends Function1[tungsten.Module, tungsten.Module] 
{
  def apply(module: tungsten.Module): tungsten.Module = {
    val functions = module.definitions.valuesIterator.collect { case f: tungsten.Function => f }
    (module /: functions) { (module, function) =>
      val blocks = module.getBlocks(function.blocks)
      val graph = function.controlFlowGraphWithCatchBlocks(module)
      val analysis = new PhiAnalysis(module)
      val phiData = analysis(graph, function.blocks.headOption)
      (module /: blocks) { (module, block) => 
        val argumentMap = argumentMapFromData(block.name, graph, phiData)
        val phiBindings = phiBindingsFromArgumentMap(argumentMap)
        val constantMap = constantMapFromPhiBindings(block.parameters, phiBindings)
        rewrite(block, phiBindings, constantMap, module) 
      }
    }
  }

  def isConstant(bindings: List[(tungsten.Value, Symbol)]): Boolean = {
    bindings.map(_._1) match {
      case Nil => false
      case h :: t => t.forall(_ == h)
    }
  }

  /** Replaces defined values with the corresponding value in the constant map (if it 
   *  exists). Use mapValues with this function if you want it to work recursively in
   *  aggregate values.
   */
  def replaceConstants(value: tungsten.Value, 
                       constants: Map[Symbol, tungsten.Value]): tungsten.Value = 
  {
    value match {
      case tungsten.DefinedValue(name, ty) => constants.getOrElse(name, value)
      case _ => value
    }
  }

  def argumentMapFromData(blockName: Symbol,
                          graph: Graph[Symbol],
                          phiData: Map[(Symbol, Symbol), List[tungsten.Value]]): Map[Symbol, List[tungsten.Value]] =
  {
    val predecessorNames = graph.incident(blockName)
    val emptyMap = Map[Symbol, List[tungsten.Value]]()
    (emptyMap /: predecessorNames) { (argumentMap, predecessorName) =>
      val arguments = phiData((predecessorName, blockName))
      argumentMap + (predecessorName -> arguments)
    }
  }

  /** Returns bindings in the same format as Phi instructions require.
   *  @param argumentMap map from predecessor name to a list of live-in variables
   *  @return a list of bindings for each parameter
   */
  def phiBindingsFromArgumentMap(argumentMap: Map[Symbol, List[tungsten.Value]]): List[List[(tungsten.Value, Symbol)]] =
  {
    if (argumentMap.isEmpty)
      Nil
    else {
      val numParameters = argumentMap.values.head.size
      val emptyPhiBindings = List.fill(numParameters)(List[(tungsten.Value, Symbol)]())
      (emptyPhiBindings /: argumentMap) { (phiBindings, kv) =>
        val (blockName, arguments) = kv
        assert(arguments.size == numParameters)
        (phiBindings zip arguments).map { pair =>
          val (bindings, argument) = pair
          (argument, blockName) :: bindings
        }
      }.map(_.reverse)
    }
  }

  /** Returns a partial map of parameter names to constant values. A parameter has a constant
   *  value if it receives the same value from all predecessors. The returned map will only
   *  contain entries for parameters with constant values.
   *  @phiBindings list of bindings as returned by phiBindingsFromArgumentMap
   */
  def constantMapFromPhiBindings(parameterNames: List[Symbol],
                                 phiBindings: List[List[(tungsten.Value, Symbol)]]): Map[Symbol, tungsten.Value] = {
    assert(parameterNames.size == phiBindings.size)
    (Map[Symbol, tungsten.Value]() /: (parameterNames zip phiBindings)) { (constantMap, pair) =>
      val (parameterName, bindings) = pair
      if (isConstant(bindings)) {
        val constantValue = bindings.head._1
        constantMap + (parameterName -> constantValue)
      } else
        constantMap
    }
  }

  def rewrite(block: tungsten.Block,
              phiBindings: List[List[(tungsten.Value, Symbol)]],
              constantMap: Map[Symbol, tungsten.Value],
              module: tungsten.Module): tungsten.Module =
  {
    assert(block.parameters.size == phiBindings.size)
    val phiNodes = (block.parameters zip phiBindings).collect { 
      case (name, bindings) if !isConstant(bindings) => {
        val ty = module.getParameter(name).ty
        TungstenPhiInstruction(name, ty, bindings)
      }
    }

    val instructions = module.getInstructions(block.instructions)
    val rewrittenInstructions = phiNodes ++ instructions.map { instruction =>
      val rewritten = instruction.mapValues(replaceConstants(_, constantMap))
      rewritten match {
        case branch: tungsten.BranchInstruction => branch.copyWith("arguments" -> Nil)
        case cond: tungsten.ConditionalBranchInstruction =>
          cond.copyWith("trueArguments" -> Nil, "falseArguments" -> Nil)
        case _ => rewritten
      }
    }
    val rewrittenBlock = block.copyWith("parameters" -> Nil,
                                        "instructions" -> rewrittenInstructions.map(_.name))
    module.remove(block.parameters).replace((rewrittenBlock :: rewrittenInstructions): _*)
  }
}
