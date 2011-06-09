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

final case class Block(name: Symbol,
                       parameters: List[Symbol],
                       instructions: List[Symbol],
                       annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  def ty(module: Module): FunctionType = {
    val parameterTypes = module.getParameters(parameters).map(_.ty)
    FunctionType(UnitType, Nil, parameterTypes)
  }

  def successors(module: Module): Set[Block] = {
    val terminator = module.getInstruction(instructions.last)
    terminator.successors.map(module.getBlock _)
  }

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Parameter](module, parameters) ++
      validateNonEmptyComponentsOfClass[Instruction](module, instructions)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    def validateInstructions(scope: Set[Symbol], 
                             instructions: List[Symbol],
                             errors: List[CompileException]): List[CompileException] =
    {
      instructions match {
        case Nil => errors
        case h :: t => {
          val instruction = module.getInstruction(h)
          val newErrors = instruction.validateScope(module, scope) ++ errors
          val newScope = scope + h
          validateInstructions(newScope, t, newErrors)
        }
      }
    }

    validateComponentsScope(module, scope, parameters) ++
      validateInstructions(scope ++ parameters, instructions, Nil)
  }

  override def validate(module: Module) = {
    def checkTermination(insts: List[Instruction]): List[CompileException] = {
      insts match {
        case Nil => throw new RuntimeException("instructions must be non-empty")
        case i :: Nil => {
          if (i.isTerminating) 
            Nil
          else
            List(BlockTerminationException(name, getLocation))
        }
        case i :: is => {
          if (!i.isTerminating) 
            checkTermination(is)
          else
            List(EarlyTerminationException(name, i.name, getLocation))
        }
      }
    }

    super.validate(module) ++ 
      checkTermination(module.getInstructions(instructions))
  }
}
