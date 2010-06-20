package tungsten.llvm

import Utilities._

final case class Function(override name: String,
                          returnType: Type,
                          attributes: List[Attribute],
                          parameters: List[Parameter],
                          blocks: List[Block])
  extends Definition(name)
{
  override def toString = {
    val attribStr = if (attributes.isEmpty) "" else " " + attributes.mkString(" ")
    if (blocks.isEmpty) {
      val paramStr = "(" + parameters.map(_.ty).mkString(", ") + ")"
      "declare " + returnType + " " + escapeIdentifier(name) + paramStr + attribStr
    } else {
      val paramStr = "(" + parameters.mkString(", ") + ")"
      val blockStr = blocks.mkString("\n\n")
      "define " + returnType + " " + escapeIdentifier(name) + paramStr + attribStr + 
        " {\n" + blockStr + "\n}"
    }
  }
}     

final case class Parameter(name: String, ty: Type, attributes: List[Attribute]) {
  override def toString = {
    val attribStr = if (attributes.isEmpty) "" else attributes.mkString(" ") + " "
    ty + " " + attribStr + escapeIdentifier(name)
  }
}

final class Attribute(description: String) {
  override def toString = description
}
object Attribute {
  val NORETURN = new Attribute("noreturn")
  val NOUNWIND = new Attribute("nounwind")
  val ZEROEXT = new Attribute("zeroext")
}

final case class Block(name: String, instructions: List[Instruction]) {
  def successors: List[String] = {
    instructions.last match {
      case BranchInstruction(DefinedValue(name, LabelType)) => List(name)
      case _ => Nil
    }
  }

  override def toString = {
    escapeIdentifier(name.substring(1)) + ":\n  " + instructions.mkString("\n  ")
  }
}

