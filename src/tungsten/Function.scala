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
  def ty(module: Module): Type = {
    val paramTypes = for (name <- parameters;
                          val Some(param) = module.get[Parameter](name))
      yield param.ty
    val functionType = FunctionType(returnType, paramTypes, location)
    if (typeParameters.isEmpty)
      functionType
    else
      UniversalType(typeParameters, functionType, location)
  }

  def validate(module: Module) = {
    typeParameters.flatMap(validateComponent[TypeParameter](module, _)) ++
      parameters.flatMap(validateComponent[Parameter](module, _)) ++
      returnType.validate(module) ++
      blocks.flatMap(validateComponent[Block](module, _))
  }
}
