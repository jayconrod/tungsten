package tungsten

import Utilities._

final case class Function(override name: Symbol,
                          typeParameters: List[Symbol],
                          parameters: List[Symbol],
                          returnType: Type,
                          blocks: List[Symbol],
                          override location: Location = Nowhere)
  extends Definition(name, location)
{
  def ty(module: Module): FunctionType = {
    FunctionType(returnType, module.getParameters(parameters).map(_.ty))
    // TODO: include type parameters
  }

  def validateComponents(module: Module) = {
    stage(validateComponentsOfClass[TypeParameter](module, typeParameters),
          validateComponentsOfClass[Parameter](module, parameters),
          returnType.validate(module),
          validateComponentsOfClass[Block](module, blocks))
  }

  def validate(module: Module) = {
    def validateReturnType = {
      blocks flatMap { blockName =>
        val block = module.get[Block](blockName).get
        block.instructions.lastOption match {
          case Some(retName) => module.get[ReturnInstruction](retName) match {
            case Some(ret) => {          
              val retTy = ret.value.ty(module)
              if (returnType != retTy)
                List(TypeMismatchException(retTy.toString, returnType.toString, ret.location))
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
          val entry = module.get[Block](entryName).get
          if (entry.parameters.isEmpty)
            Nil
          else
            List(EntryParametersException(name, entry.name, location))
        }
      }
    }

    def validateBranches = {
      blocks flatMap { blockName =>
        val block = module.get[Block](blockName).get
        block.instructions flatMap { instName =>
          val inst = module.get[Instruction](instName)
          val blockNames = inst match {
            case Some(BranchInstruction(_, target, _, _)) => List(target)
            case Some(ConditionalBranchInstruction(_, _, trueTarget, falseTarget, _, _)) =>
              List(trueTarget, falseTarget)
            case _ => Nil
          }
          blockNames flatMap { n =>
            if (!blocks.contains(n)) {
              module.getDefn(n) match {
                case Some(_) => List(NonLocalBranchException(name, n, inst.get.location))
                case None => List(UndefinedSymbolException(n, inst.get.location))
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
            val instNames = i.operandSymbols
            val invalidNames = instNames.diff(validNames.toList)
            val newErrors = invalidNames.map(InstructionOrderException(_, i.location))
            checkOrder(is, validNames + i.name, newErrors ++ errors)
          }
        }
      }
      def checkBlock(blockName: Symbol) = {
        val block = module.get[Block](blockName).get
        val validNames = (parameters ++ block.parameters).toSet
        val insts = block.instructions.map(module.get[Instruction](_).get)
        checkOrder(insts, validNames, Nil)
      }
      blocks.flatMap(checkBlock _)
    }
    
    stage(validateEntryParameters,
          validateInstructionOrder,
          validateBranches,
          validateReturnType)
  }
}
