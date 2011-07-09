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

    def validateExceptionHandlers = {
      blocks.flatMap { blockName =>
        val block = module.getBlock(blockName)
        block.catchBlock match {
          case Some((handlerName, arguments)) => {
            val argumentTypes = arguments.map(_.ty)
            val handler = module.getBlock(handlerName)
            val handlerParameterTypes = module.getParameters(handler.parameters).map(_.ty)
            handlerParameterTypes match {
              case exnType :: paramTypes => {
                val exnTypeErrors = if (exnType != ClassType("tungsten.Exception"))
                  List(InvalidExceptionHandlerException(blockName, handlerName, block.getLocation))
                else
                  Nil
                val paramTypeErrors = if (argumentTypes.size != paramTypes.size)
                  List(FunctionArgumentCountException(handlerName, argumentTypes.size, paramTypes.size, block.getLocation))
                else {
                  (argumentTypes zip paramTypes) flatMap { case ap =>
                    checkType(ap._1, ap._2, block.getLocation)
                  }
                }
                exnTypeErrors ++ paramTypeErrors
              }
              case Nil => List(InvalidExceptionHandlerException(blockName, handlerName, block.getLocation))
            }
          }
          case None => Nil
        }
      }
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
      val cfg = controlFlowGraph(module)
      val paramErrors = for (block <- cfg.nodes.toList;
                             if !block.parameters.isEmpty && cfg.incident(block).isEmpty)
                          yield BlockPredecessorException(block.name, block.getLocation)

      val entryErrors = for (entryName <- blocks.headOption.toList;
                             val entry = module.getBlock(entryName);
                             if !cfg.incident(entry).isEmpty)
                          yield EntryBlockPredecessorException(name, entry.name, getLocation)

      paramErrors ++ entryErrors
    }

    stage(super.validate(module),
          validateEntryParameters,
          validateExceptionHandlers,
          validateReturnType,
          validateVariance,
          validateAbstract,
          validatePredecessors)
  }

  def controlFlowGraph(module: Module): Graph[Block] = {
    val blockDefns = module.getBlocks(blocks)
    val successorMap = (Map[Block, Set[Block]]() /: blockDefns) { (deps, defn) =>
      deps + (defn -> defn.successors(module))
    }
    new Graph(blockDefns, successorMap)
  }
}
