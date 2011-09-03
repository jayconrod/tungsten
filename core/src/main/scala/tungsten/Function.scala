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

import Utilities._

final case class Function(name: Symbol,
                          returnType: Type,
                          typeParameters: List[Symbol],
                          parameters: List[Symbol],
                          blocks: List[Symbol],
                          annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def isGlobal = true

  def ty(module: Module): FunctionType = {
    FunctionType(returnType, 
                 typeParameters,
                 module.getParameters(parameters).map(_.ty))
  }

  def isDefined = !blocks.isEmpty

  def isVariadic(module: Module): Boolean = {
    if (parameters.isEmpty)
      false
    else
      module.getParameter(parameters.last).ty == VariadicType
  }

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[TypeParameter](module, typeParameters) ++
      validateComponentsOfClass[Parameter](module, parameters) ++
      validateComponentsOfClass[Block](module, blocks)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    val tpErrors = validateComponentsScope(module, scope, typeParameters)
    validateTypeAndValueScope(scope ++ typeParameters) ++
      validateComponentsScope(module, scope ++ typeParameters, parameters) ++
      validateComponentsScope(module, scope ++ typeParameters ++ parameters ++ blocks, blocks)
  }

  override def validate(module: Module) = {
    def validateReturnType = {
      blocks flatMap { blockName =>
        val block = module.getBlock(blockName)
        block.instructions.lastOption match {
          case Some(retName) => module.get[ReturnInstruction](retName) match {
            case Some(ret) => {          
              val retTy = ret.value.ty
              if (returnType != retTy)
                List(TypeMismatchException(retTy.toString, returnType.toString, ret.getLocation))
              else
                Nil
            }
            case None => Nil
          }
          case None => Nil
        }
      }
    }

    def validateParameters = {
      val paramTypes = module.getParameters(parameters).map(_.ty)
      val variadicIndex = paramTypes.indexOf(VariadicType)
      if (variadicIndex != -1 && variadicIndex < parameters.size - 1) {
        val parameter = module.getParameter(parameters(variadicIndex))
        List(VariadicFunctionException(name, parameter.name, parameter.getLocation))
      } else
        Nil
    }

    def validateEntryParameters = {
      blocks match {
        case Nil => Nil
        case entryName :: _ => {
          val entry = module.getBlock(entryName)
          if (entry.parameters.isEmpty)
            Nil
          else
            List(EntryParametersException(name, entry.name, getLocation))
        }
      }
    }

    def validateEntryCatch = {
      var errors = List[CompileException]()
      blocks.headOption match {
        case Some(entryName) => {
          val entry = module.getBlock(blocks.head)
          entry.catchBlock match {
            case Some((handler, _)) => errors ::= InvalidCatchBlockException(entry.name, handler, entry.getLocation)
            case _ => ()
          }
          if (entry.isCatchBlock(module))
            errors ::= CatchEntryException(entry.name, entry.getLocation)
        }
        case None => ()
      }
      errors
    }

    def validateVariance = {
      val parameterTypes = module.getParameters(parameters).map(_.ty)
      returnType.validateVariance(Variance.COVARIANT, module, getLocation) ++
        parameterTypes.flatMap(_.validateVariance(Variance.CONTRAVARIANT, module, getLocation))
    }

    def validateAbstract = {
      if (isAbstract && isDefined)
        List(AbstractMethodDefinedException(name, getLocation))
      else if (isAbstract && isFinal)
        List(AbstractFinalMethodException(name, getLocation))
      else
        Nil
    }

    def validatePredecessors = {
      val cfg = controlFlowGraphWithCatchBlocks(module)
      val blockDefns = module.getBlocks(blocks)
      val paramErrors = for (block <- blockDefns;
                             if !block.parameters.isEmpty && cfg.incident(block.name).isEmpty)
                          yield BlockPredecessorException(block.name, block.getLocation)

      val entryErrors = for (entryName <- blocks.headOption.toList;
                             val entry = module.getBlock(entryName);
                             if !cfg.incident(entry.name).isEmpty)
                          yield EntryBlockPredecessorException(name, entry.name, getLocation)

      paramErrors ++ entryErrors
    }

    stage(super.validate(module),
          validateParameters,
          validateEntryParameters,
          validateEntryCatch,
          validateReturnType,
          validateVariance,
          validateAbstract,
          validatePredecessors)
  }

  def controlFlowGraph(module: Module): Graph[Symbol] = {
    val blockDefns = module.getBlocks(blocks)
    val successorMap = (Map[Symbol, Set[Symbol]]() /: blockDefns) { (deps, defn) =>
      deps + (defn.name -> defn.successorNames(module))
    }
    new Graph(blocks.toSet, successorMap)
  }

  def controlFlowGraphWithCatchBlocks(module: Module): Graph[Symbol] = {
    val blockDefns = module.getBlocks(blocks)
    val successorMap = blockDefns.map { block =>
      val successorNames = block.successorNames(module) ++ block.catchBlock.map(_._1)
      (block.name, successorNames)
    }.toMap
    new Graph(blocks.toSet, successorMap)
  }
}
