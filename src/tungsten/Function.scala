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

    stage(validateComponents[TypeParameter](module, typeParameters),
          validateComponents[Parameter](module, parameters),
          returnType.validate(module),
          validateComponents[Block](module, blocks),
          validateReturnType)
  }
}
