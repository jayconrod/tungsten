package tungsten

import Utilities._

final case class Struct(name: Symbol,
                        fields: List[Symbol],
                        annotations: List[AnnotationValue] = Nil)
  extends Definition
{
  override def isGlobal = true

  def size(module: Module): Long = {
    val fieldSizes = module.getFields(fields).map(_.ty.size(module))
    fieldSizes.sum
  }

  override def validateComponents(module: Module) = {
    super.validateComponents(module) ++ 
      validateComponentsOfClass[Field](module, fields)
  }

  override def validateScope(module: Module, scope: Set[Symbol]): List[CompileException] = {
    validateComponentsScope(module, scope, fields)
  }
}
