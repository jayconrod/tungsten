package tungsten

import Utilities._

final case class Function(name: Symbol,
                          parameters: List[Symbol],
                          returnType: Type,
                          blocks: List[Symbol],
                          annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  def ty(module: Module): FunctionType = {
    FunctionType(returnType, module.getParameters(parameters).map(_.ty))
    // TODO: include type parameters
  }

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Parameter](module, parameters) ++
      returnType.validate(module, getLocation) ++
      validateComponentsOfClass[Block](module, blocks)
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

    def validateBranches = {
      blocks flatMap { blockName =>
        val block = module.getBlock(blockName)
        block.instructions flatMap { instName =>
          val inst = module.getInstruction(instName)
          val blockNames = inst match {
            case BranchInstruction(_, _, target, _, _) => List(target)
            case ConditionalBranchInstruction(_, _, _, trueTarget, _, falseTarget, _, _) =>
              List(trueTarget, falseTarget)
            case _ => Nil
          }
          blockNames flatMap { n =>
            if (!blocks.contains(n)) {
              module.getDefn(n) match {
                case Some(_) => List(NonLocalBranchException(name, n, inst.getLocation))
                case None => List(UndefinedSymbolException(n, inst.getLocation))
              }
            } else
              Nil
          }
        }
      }
    }

    def validateInstructionOrder = {
      def checkOrder(insts: List[Instruction],
                     validNames: Set[Symbol],
                     errors: List[CompileException]): List[CompileException] =
      {
        insts match {
          case Nil => errors
          case i :: is => {
            val instNames = i.operandSymbols.toSet
            val invalidNames = instNames &~ validNames
            val newErrors = invalidNames.toList.map(InstructionOrderException(_, i.getLocation))
            checkOrder(is, validNames + i.name, newErrors ++ errors)
          }
        }
      }
      val globalNames = module.definitions.values.
                        filter(_.isInstanceOf[Global]).map(_.name).toSet
      def checkBlock(blockName: Symbol) = {
        val block = module.getBlock(blockName)
        val validNames = (parameters ++ block.parameters).toSet union globalNames
        val insts = module.getInstructions(block.instructions)
        checkOrder(insts, validNames, Nil)
      }
      blocks.flatMap(checkBlock _)
    }
    
    stage(super.validate(module),
          validateEntryParameters,
          validateInstructionOrder,
          validateBranches,
          validateReturnType)
  }
}
