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
    typeParameters.flatMap(validateComponent[TypeParameter](module, _)) ++
      parameters.flatMap(validateComponent[Parameter](module, _)) ++
      returnType.validate(module) ++
      blocks.flatMap(validateComponent[Block](module, _))
  }
}
