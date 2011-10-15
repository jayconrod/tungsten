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

/** A function which performs a transformation on a module. */
trait Pass 
  extends Function1[Module, Module]
{
  /** The user may give this to w-opt to specify which passes to run */
  def name: String

  /** A short description for w-opt to give to the user */
  def description: String
  
  def apply(module: Module): Module = {
    symbolFactory = new SymbolFactory(module.highestSymbolId + 1)
    processModule(module)
  }

  /** The function that actually implements the pass */
  def processModule(module: Module): Module

  /** Most passes need to generate new symbols, so this factory is used to generate symbols
   *  with unique IDs. It is re-initialized in apply for each module processed. A default
   *  value is provided for units tests.
   */
  protected var symbolFactory = new SymbolFactory
}

/** Transforms each function in a module */
trait FunctionPass
  extends Pass
{
  def processFunction(function: Function, module: Module): Module

  def processModule(module: Module): Module = {
    val functions = module.definitions.values.collect { case f: Function => f }
    var m = module
    functions.foreach { f => m = processFunction(f, m) }
    m
  }
}

/** Rewrites the instructions in each function in a module. May change control flow by
 *  splitting and adding blocks, but cannot delete blocks.
 */
trait InstructionRewritePass
  extends FunctionPass
{
  def processFunction(function: Function, module: Module): Module = {
    val reversedBlocks = module.getBlocks(function.blocks).reverse
    val (rewrittenBlocks, newModule) = rewriteBlocks(reversedBlocks, Nil, module)
    val newFunction = function.copy(blocks = rewrittenBlocks.map(_.name))
    newModule.replace(newFunction)
  }

  def rewriteBlocks(inBlocks: List[Block],
                    outBlocks: List[Block],
                    module: Module): (List[Block], Module) =
  {
    inBlocks match {
      case Nil => (outBlocks, module)
      case next :: rest => {
        val instructions = module.getInstructions(next.instructions)
        val (rewrittenBlocks, newModule) = rewriteInstructions(instructions.reverse, Nil,
                                                         next, outBlocks, module)
        rewriteBlocks(rest, rewrittenBlocks, newModule)
      }
    }
  }

  /** Subclasses of InstructionRewritePass must implement rewriteInstruction, which returns
   *  either RewrittenInstructions or SplitBlock. The return value determines how control
   *  flow is reorganized in the block.
   */
  sealed abstract class RewriteResult

  /** In the simplest case, rewriteInstruction will return RewrittenInstructions. This allows
   *  the implementor to map an instruction to a (possibly) empty sequence of instructions to
   *  replace it.
   */
  case class RewrittenInstructions(rewritten: List[Instruction])
    extends RewriteResult

  /** rewriteInstruction can return SplitBlock when it needs to modify control flow. The
   *  current block will be split after outInstructions is added. Instructions which are
   *  rewritten from other instructions will be added to a new block.
   *  @param outInstructions these will be added to the list of instructions for the current
   *    block. Since the block will be cut off, these will end up as the first instructions
   *    in the block. These do not need to be in the module (they will be added).
   *  @param newBlocks new blocks to be added to the function. These blocks must have already
   *    been added to the module.
   *  @param module a modified module. Must contain newBlocks and any definitions they refer to.
   *  @param continuation after the block has been split, this function will be called back
   *    with the name of the block, a list of arguments to call it with, and a modified module
   *    containing the block. The continuation must return a RewriteResult. Any newly
   *    rewritten instructions will be added to a new block. The rewritten instructions
   *    must branch to the new block (otherwise it will be orphaned).
   */
  case class SplitBlock(outInstructions: List[Instruction],
                        newBlocks: List[Block],
                        module: Module,
                        continuation: (Symbol, List[Value], Module)=>RewriteResult)
    extends RewriteResult

  /** Rewrites the instructions in a block. Note that instructions are processed in reverse
   *  order, since this makes it easier to split the block into smaller blocks when needed.
   *  @param inInstructions the instructions to rewrite in reverse order
   *  @param outInstructions instructions that have been rewritten so far
   *  @param inBlock the original block currently being rewritten
   *  @param outBlocks blocks which have been rewritten so far
   *  @return the list out output blocks is returned along with a module once all instructions
   *    have been processed. The module will contain everything rewritten so far.
   */
  def rewriteInstructions(inInstructions: List[Instruction],
                          outInstructions: List[Instruction],
                          inBlock: Block,
                          outBlocks: List[Block],
                          module: Module): (List[Block], Module) =
  {
    /** Creates a new block based on the current block containing the instructions rewritten
     *  so far. References to parameters or instructions earlier in the block (not rewritten
     *  yet) will be replaced with new parameters.
     *  @param instructions the instructions to include in the new block
     *  @return the new block, a list of arguments to call it with, and a module containing
     *    the block, the substituted instructions, and the new parameters
     */
    def splitBlock(instructions: List[Instruction],
                   module: Module): (Block, List[Value], Module) = 
    {
      // Calculate the set of live-in symbols for the current block: values which are 
      // used in this block (either by instructions or the catch arguments) but are 
      // defined in a predecessor. 
      val parameterSet = inBlock.parameters.toSet
      val catchLive: Set[Symbol] = inBlock.catchBlock match {
        case Some((_, arguments)) => {
          (Set[Symbol]() /: arguments) { (live, arg) =>
            live ++ arg.getSymbols.filter(parameterSet.contains _)
          }
        }
        case None => Set()
      }
      val live = (catchLive /: instructions.reverse) { (live, inst) =>
        val uses = inst.operandSymbols.filter { sym =>
          parameterSet.contains(sym) || module.definitions(sym).isInstanceOf[Instruction]
        }
        live ++ uses - inst.name
      }

      // Create a new list of parameters for the live-in symbols. Predecessors will pass
      // the live-in values as arguments for these parameters.
      var liveParameters: List[Parameter] = Nil
      var liveArguments: List[Value] = Nil
      var substitutions: Map[Symbol, Symbol] = Map()
      live.foreach { sym =>
        val value = module.definitions(sym) match {
          case p: Parameter => p.makeValue
          case i: Instruction => i.makeValue
          case _ => throw new RuntimeException("illegal live symbol")
        }
        val param = Parameter(symbolFactory(sym), value.ty)
        liveParameters = param :: liveParameters
        liveArguments = DefinedValue(sym, param.ty) :: liveArguments
        substitutions = substitutions + (sym -> param.name)
      }

      // Substitute uses of live-in symbols with the new parameters and build the block
      val substitutedInsts = instructions.map(_.substituteSymbols(substitutions))
      val substitutedCatchBlock = inBlock.catchBlock.map { cb =>
        cb.copy(_2 = cb._2.map(_.substituteSymbols(substitutions)))
      }
      val newBlock = Block(symbolFactory(inBlock.name),
                           liveParameters.map(_.name),
                           substitutedInsts.map(_.name),
                           substitutedCatchBlock,
                           inBlock.annotations)
      val newModule = module.
        replace(newBlock).
        replace(liveParameters: _*).
        replace(substitutedInsts: _*)
      (newBlock, liveArguments, newModule)
    }

    inInstructions match {
      case Nil => {
        val newBlock = inBlock.copy(instructions = outInstructions.map(_.name))
        (newBlock :: outBlocks, module.replace(newBlock))
      }
      case next :: rest => {
        // Sorry for ugly control flow here. Each time SplitBlock is returned, we need to
        // call the continuation and process another result. This allows a single instruction
        // to split the block multiple times.
        var m = module
        var blocks = outBlocks
        var rewritten = outInstructions
        var result = rewriteInstruction(next, inBlock, m)
        var ret: Option[(List[Block], Module)] = None
        while (!ret.isDefined) {
          result match {
            case SplitBlock(out, newBlocks, splitModule, continuation) => {
              val (oldBlock, liveOutArguments, m_1) = splitBlock(out ++ rewritten, splitModule)
              m = m_1
              blocks = oldBlock :: newBlocks ++ blocks
              rewritten = Nil
              result = continuation(oldBlock.name, liveOutArguments, m)
            }
            case RewrittenInstructions(out) => {
              m = m.replace(out: _*)
              ret = Some(rewriteInstructions(rest, out ++ rewritten,
                                             inBlock, blocks, m))
            }
          }
        }
        ret.get
      }
    }
  }

  /** Rewrites an instruction into zero or more instructions. In the simple case,
   *  RewrittenInstructions should be returned, which just translates an instruction into
   *  a sequence of instructions in the same block. More complicated cases can return
   *  SplitBlock (see comments for that). Rewritten instructions need not be added to any
   *  module; they will be added automatically by the caller.
   */
  def rewriteInstruction(instruction: Instruction,
                         block: Block,
                         module: Module): RewriteResult
}

/** Internal passes are used to implement other passes. They should not be made available
 *  to the user.
 */
trait InternalPass
  extends Pass
{
  def name = throw new UnsupportedOperationException
  def description = throw new UnsupportedOperationException
}
