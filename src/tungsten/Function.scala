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
    def validateComponents = {
      typeParameters.flatMap(validateComponent[TypeParameter](module, _)) ++
        parameters.flatMap(validateComponent[Parameter](module, _)) ++
        returnType.validate(module) ++
        blocks.flatMap(validateComponent[Block](module, _))
    }

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

    validateComponents ++ validateReturnType
  }
}
