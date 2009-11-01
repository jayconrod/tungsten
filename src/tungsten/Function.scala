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
    FunctionType(returnType, parameters.map(module.get[Parameter](_).get.ty))
    // TODO: include type parameters
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

    def validateBlocks = {
      def validateParameters(blockName: Symbol) = {
        val block = module.get[Block](blockName).get
        validateComponents[Parameter](module, block.parameters)
      }
      stage(validateComponents[Block](module, blocks),
            blocks flatMap validateParameters)
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

    stage(validateComponents[TypeParameter](module, typeParameters),
          validateComponents[Parameter](module, parameters),
          returnType.validate(module),
          validateBlocks,
          validateBranches,
          validateReturnType)
  }
}
