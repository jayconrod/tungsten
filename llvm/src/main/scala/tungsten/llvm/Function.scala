package tungsten.llvm

import Utilities._

final case class Function(override val name: String,
                          returnAttributes: Set[ParameterAttribute],
                          returnType: Type,
                          parameters: List[Parameter],
                          functionAttributes: Set[FunctionAttribute],
                          blocks: List[Block])
  extends Definition(name)
{
  override def toString = {
    val retAttribStr = if (returnAttributes.isEmpty) "" else " " + returnAttributes.mkString(" ")
    val fnAttribStr = if (functionAttributes.isEmpty) "" else " " + functionAttributes.mkString(" ")
    if (blocks.isEmpty) {
      val paramStr = "(" + parameters.map(_.ty).mkString(", ") + ")"
      "declare %s%s %s%s%s".format(retAttribStr, returnType,
                                   escapeIdentifier(name), paramStr, fnAttribStr)
    } else {
      val paramStr = "(" + parameters.mkString(", ") + ")"
      val blockStr = blocks.mkString("\n\n")
      "define %s%s %s%s%s {\n%s\n}".format(retAttribStr, returnType,
                                           escapeIdentifier(name), paramStr, fnAttribStr,
                                           blockStr)
    }
  }
}     

final case class Parameter(name: String, ty: Type, attributes: Set[ParameterAttribute]) {
  override def toString = {
    val attribStr = if (attributes.isEmpty) "" else attributes.mkString(" ") + " "
    ty + " " + attribStr + escapeIdentifier(name)
  }
}

final class ParameterAttribute(description: String) {
  override def toString = description
}
object ParameterAttribute {
  val ZEROEXT = new ParameterAttribute("zeroext")
  val SIGNEXT = new ParameterAttribute("signext")
  val INREG = new ParameterAttribute("inreg")
  val BYVAL = new ParameterAttribute("byval")
  val SRET = new ParameterAttribute("sret")
  val NOALIAS = new ParameterAttribute("noalias")
  val NOCAPTURE = new ParameterAttribute("nocapture")
  val NEST = new ParameterAttribute("nest")
}

final class FunctionAttribute(description: String) {
  override def toString = description
}
object FunctionAttribute {
  val ALWAYSINLINE = new FunctionAttribute("alwaysinline")
  val INLINEHINT = new FunctionAttribute("inlinehint")
  val OPTSIZE = new FunctionAttribute("optsize")
  val NORETURN = new FunctionAttribute("noreturn")
  val NOUNWIND = new FunctionAttribute("nounwind")
  val READNONE = new FunctionAttribute("readnone")
  val READONLY = new FunctionAttribute("readonly")
  val SSP = new FunctionAttribute("ssp")
  val SSPREQ = new FunctionAttribute("sspreq")
  val NOREDZONE = new FunctionAttribute("noredzone")
  val NOIMPLICITFLOAT = new FunctionAttribute("noimplicitfloat")
  val NAKED = new FunctionAttribute("naked")
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

