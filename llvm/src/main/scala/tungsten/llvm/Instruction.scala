package tungsten.llvm

import Utilities._

sealed abstract class Instruction(val name: String) {
  def ty(module: Module): Type
  def operands: List[Value]
  def usedVars: List[String] = operands collect { 
    case DefinedValue(name, ty) if ty != LabelType => name 
  }
}

final case class AllocaInstruction(override name: String,
                                   elementType: Type)
  extends Instruction(name)
{
  def ty(module: Module) = PointerType(elementType)
  def operands = Nil
  override def toString = escapeIdentifier(name) + " = alloca " + elementType
}

final case class BitcastInstruction(override name: String,
                                    value: Value,
                                    ty: Type)
  extends Instruction(name)
{
  def ty(module: Module) = ty
  def operands = List(value)
  override def toString = escapeIdentifier(name) + " = bitcast " + value.typedToString + " to " + ty
}

final case class BranchInstruction(label: Value)
  extends Instruction("")
{
  def ty(module: Module) = VoidType
  def operands = List(label)
  override def toString = "br " + label
}

final case class GetElementPointerInstruction(override name: String,
                                              base: Value,
                                              indices: List[Value])
  extends Instruction(name)
{
  def ty(module: Module) = {
    def getStructFieldType(structType: Type, index: Value): Type = {
      val ix = index match {
        case IntValue(i, 32) => i
        case _ => throw new LlvmConversionException("invalid index value for struct: " + index)
      }
      val fieldTypes = structType match {
        case StructType(fts) => fts
        case NamedStructType(name) => {
          module.definitions.get(name) match {
            case Some(Struct(_, fts)) => fts
            case _ => throw new LlvmConversionException("struct name %s does not correspond to a defined struct type".format(name))
          }
        }
        case _ => throw new RuntimeException("struct type expected")
      }
      if (0 <= ix && ix < fieldTypes.size)
        fieldTypes(ix.toInt)
      else
        throw new LlvmConversionException("struct type %s does not have a field at index %d".format(structType, ix))
    }

    def getElementType(baseType: Type, indices: List[Value]): Type = {
      indices match {
        case Nil => baseType
        case i :: is => {
          val elementType = baseType match {
            case _: StructType | _: NamedStructType => getStructFieldType(baseType, i)
            case ArrayType(_, elementType) => elementType
            case _ => throw new LlvmConversionException("type %s is not indexable".format(baseType))
          }
          getElementType(elementType, is)
        }
      }
    }

    val baseType = base.ty match {
      case PointerType(t) => t
      case _ => throw new LlvmConversionException("base %s is not a pointer".format(base))
    }
    val elementType = getElementType(baseType, indices.tail)
    PointerType(elementType)
  }

  def operands = base :: indices
}
    
final case class LoadInstruction(override name: String,
                                 address: Value,
                                 alignment: Option[Int])
  extends Instruction(name)
{
  def ty(module: Module) = address.ty.asInstanceOf[PointerType].elementType
  def operands = List(address)
  override def toString = {
    val alignmentStr = alignment.map(", " + _).getOrElse("")
    escapeIdentifier(name) + " = load " + address.typedToString  + alignmentStr
  }
}

final case class PhiInstruction(override name: String,
                                ty: Type,
                                bindings: List[(Value, String)])
  extends Instruction(name)
{
  def ty(module: Module) = ty
  def operands = Nil
  override def toString = {
    val bindingsStrs = bindings.map { b => 
      "[" + b._1 + ", " + escapeIdentifier(b._2) + "]"
    }
    name + " = phi " + ty + bindingsStrs.mkString(", ")
  }
}

final case class ReturnInstruction(value: Value)
  extends Instruction("")
{
  def ty(module: Module) = VoidType
  def operands = List(value)
  override def toString = "ret " + value.typedToString
}

final case class StoreInstruction(value: Value,
                                  address: Value,
                                  alignment: Option[Int])
  extends Instruction("")
{
  def ty(module: Module) = VoidType
  def operands = List(value, address)
  override def toString = {
    val alignmentStr = alignment.map(", align " + _).getOrElse("")
    "store " + value.typedToString + ", " + address.typedToString + alignmentStr
  }
}
