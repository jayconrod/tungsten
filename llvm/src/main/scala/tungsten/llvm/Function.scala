package tungsten.llvm

final case class Function(name: String,
                          returnType: Type,
                          attributes: List[Attribute],
                          parameters: List[Parameter],
                          blocks: List[Block])
  extends Definition

final case class Parameter(name: String, ty: Type, attributes: List[Attribute])

final class Attribute(description: String)
object Attribute {
  val NOUNWIND = new Attribute("nounwind")
}

final case class Block(name: String, instructions: List[Instruction])
