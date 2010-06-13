package tungsten.llvm

import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverter(module: tungsten.Module) {
  def convert: Module = {
    val dataLayoutFmt = "e-p:%s-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
    val wordSize = if (module.is64Bit) 64 else 32
    val dataLayout = Some(dataLayoutFmt.format(wordSize + ":" + wordSize + ":" + wordSize))
    val cDefinitions = (Map[String, Definition]() /: module.definitions.values) { (defns, defn) =>
      val cDefn = defn match {
        case s: tungsten.Struct => Some(convertStruct(s))
        case g: tungsten.Global => Some(convertGlobal(g))
        case f: tungsten.Function => Some(convertFunction(f))
        case _ => None
      }
      cDefn match {
        case Some(d) => defns + (d.name -> d)
        case None => defns
      }
    }
    new Module(dataLayout, None, cDefinitions)
  }

  def convertGlobal(global: tungsten.Global): Global = {
    val cName = globalSymbol(global.name)
    val cValue = global.value match {
      case Some(v) => convertValue(v, None)
      case None => {
        val v = global.ty.defaultValue(module)
        convertValue(v, None)
      }
    }
    Global(cName, Nil, cValue)
  }

  def convertStruct(struct: tungsten.Struct): Struct = {
    val cName = '%' + globalSymbol(struct.name).substring(1)
    val fields = module.getFields(struct.fields)
    val cFieldTypes = fields.map { f: tungsten.Field => convertType(f.ty) }
    Struct(cName, cFieldTypes)
  }

  def convertFunction(function: tungsten.Function): Function = {
    val cName = globalSymbol(function.name)
    val cReturnType = convertType(function.returnType)
    val parameters = module.getParameters(function.parameters)
    val cParameters = parameters.map(convertParameter(_, Some(function.name)))
    val blocks = module.getBlocks(function.blocks)
    val cBlocks = blocks.map(convertBlock(_, Some(function.name)))
    Function(cName, cReturnType, Nil, cParameters, cBlocks)
  }

  def convertParameter(parameter: tungsten.Parameter, parent: Option[Symbol]): Parameter = {
    val cName = localSymbol(parameter.name, parent)
    val cType = convertType(parameter.ty)
    Parameter(cName, cType, Nil)
  }

  def convertBlock(block: tungsten.Block, parent: Option[Symbol]): Block = {
    assert(block.parameters.isEmpty)
    val cName = localSymbol(block.name, parent)
    val instructions = module.getInstructions(block.instructions)
    val cInstructions = instructions.map(convertInstruction(_, parent))
    Block(cName, cInstructions)
  }

  def convertInstruction(instruction: tungsten.Instruction, 
                         parent: Option[Symbol]): Instruction = 
  {
    val localName = localSymbol(instruction.name, parent)
    instruction match {
      case tungsten.AddressInstruction(_, _, base, indices, _) => {
        GetElementPointerInstruction(localName, 
                                     convertValue(base, parent), 
                                     indices.map(convert32BitValue(_, parent)))
      }
      case tungsten.AssignInstruction(_, ty, value, _) =>
        BitCastInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.BinaryOperatorInstruction(_, ty, op, left, right, _) => {
        import tungsten.BinaryOperator._
        assert(left.ty == right.ty)
        val cTy = convertType(left.ty)
        val cLeft = convertValue(left, parent)
        val cRight = convertValue(right, parent)
        val instCtor = if (ty.isInstanceOf[FloatType]) {
          op match {
            case MULTIPLY => FloatMultiplyInstruction.apply _
            case DIVIDE => FloatDivideInstruction.apply _
            case REMAINDER => SignedRemainderInstruction.apply _
            case ADD => AddInstruction.apply _
            case SUBTRACT => SubtractInstruction.apply _
            case _ => throw new RuntimeException("operator " + op + " does not support floating point types")
          }
        } else {
          op match {
            case MULTIPLY => MultiplyInstruction.apply _
            case DIVIDE => SignedDivideInstruction.apply _
            case REMAINDER => UnsignedRemainderInstruction.apply _
            case ADD => AddInstruction.apply _
            case SUBTRACT => SubtractInstruction.apply _
            case LEFT_SHIFT => ShiftLeftInstruction.apply _
            case RIGHT_SHIFT_ARITHMETIC => ArithmeticShiftRightInstruction.apply _
            case RIGHT_SHIFT_LOGICAL => LogicalShiftRightInstruction.apply _
            case AND => AndInstruction.apply _
            case XOR => XorInstruction.apply _
            case OR => OrInstruction.apply _
          }
        }
        instCtor(localName, cTy, cLeft, cRight)
      }
      case tungsten.BitCastInstruction(_, ty, value, _) =>
        BitCastInstruction(localName, convertValue(value, parent), convertType(ty))        
      case tungsten.BranchInstruction(_, _, target, arguments, _) => {
        assert(arguments.isEmpty)
        val cTargetName = localSymbol(target, parent)
        val cTarget = DefinedValue(cTargetName, LabelType)
        BranchInstruction(cTarget)
      }
      case tungsten.ConditionalBranchInstruction(_, _, condition, 
                                                 trueTarget, trueArguments, 
                                                 falseTarget, falseArguments, _) => 
      {
        assert(trueArguments.isEmpty && falseArguments.isEmpty)
        val cCondition = convertValue(condition, parent)
        val cTrueTargetName = localSymbol(trueTarget, parent)
        val cTrueTarget = DefinedValue(cTrueTargetName, LabelType)
        val cFalseTargetName = localSymbol(falseTarget, parent)
        val cFalseTarget = DefinedValue(cFalseTargetName, LabelType)
        ConditionalBranchInstruction(cCondition, cTrueTarget, cFalseTarget)
      }
      case tungsten.FloatExtendInstruction(_, ty, value, _) =>
        FloatExtendInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.FloatToIntegerInstruction(_, ty, value, _) =>
        FloatToSignedIntegerInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.FloatTruncateInstruction(_, ty, value, _) =>
        FloatTruncateInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.IntegerSignExtendInstruction(_, ty, value, _) =>
        IntegerSignExtendInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.IntegerToFloatInstruction(_, ty, value, _) =>
        SignedIntegerToFloatInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.IntegerTruncateInstruction(_, ty, value, _) =>
        IntegerTruncateInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.IntegerZeroExtendInstruction(_, ty, value, _) =>
        IntegerZeroExtendInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.LoadInstruction(_, _, address, _) =>
        LoadInstruction(localName, convertValue(address, parent), None)
      case tungsten.RelationalOperatorInstruction(_, _, op, left, right, _) => {
        import tungsten.RelationalOperator._
        import Comparison._
        assert(left.ty == right.ty)
        val cTy = convertType(left.ty)
        val cLeft = convertValue(left, parent)
        val cRight = convertValue(right, parent)
        val isFloat = left.ty.isInstanceOf[tungsten.FloatType]
        if (isFloat) {
          val cmp = op match {
            case EQUAL => OEQ
            case NOT_EQUAL => ONE
            case LESS_THAN => OLT
            case LESS_EQUAL => OLE
            case GREATER_THAN => OGT
            case GREATER_EQUAL => OGE
          }
          FloatCompareInstruction(localName, cmp, cTy, cLeft, cRight)
        } else {
          val cmp = op match {
            case EQUAL => EQ
            case NOT_EQUAL => NE
            case LESS_THAN => SLT
            case LESS_EQUAL => SLE
            case GREATER_THAN => SGT
            case GREATER_EQUAL => SGE
          }
          IntegerCompareInstruction(localName, cmp, cTy, cLeft, cRight)
        }
      }
      case tungsten.ReturnInstruction(_, _, value, _) =>
        ReturnInstruction(convertValue(value, parent))
      case tungsten.StoreInstruction(_, _, value, address, _) =>
        StoreInstruction(convertValue(value, parent), convertValue(address, parent), None)
      case tungsten.StaticCallInstruction(_, ty, target, arguments, _) => {
        val cReturnType = convertType(ty)
        val cArgumentTypes = arguments.map { a: tungsten.Value => convertType(a.ty) }
        val cTargetType = FunctionType(cReturnType, cArgumentTypes)
        CallInstruction(localName, false, None, Nil, 
                        cReturnType, None,
                        DefinedValue(globalSymbol(target), cTargetType), 
                        arguments.map(convertValue(_, parent)), Nil)
      }
      case tungsten.UpcastInstruction(_, ty, value, _) =>
        BitCastInstruction(localName, convertValue(value, parent), convertType(ty))
      case TungstenPhiInstruction(_, ty, bindings) => {
        val cTy = convertType(ty)
        val cBindings = bindings.map { vb =>
          val (value, blockName) = vb
          val cValue = convertValue(value, parent)
          val cBlockName = localSymbol(blockName, parent)
          (cValue, cBlockName)
        }
        PhiInstruction(localName, cTy, cBindings)
      }
      case _ => throw new UnsupportedOperationException // TODO
    }
  }

  def convertValue(value: tungsten.Value, parent: Option[Symbol]): Value = {
    value match {
      case tungsten.UnitValue => VoidValue
      case tungsten.BooleanValue(true) => IntValue(1L, 1)
      case tungsten.BooleanValue(false) => IntValue(0L, 1)
      case tungsten.IntValue(value, width) => IntValue(value, width)
      case tungsten.FloatValue(value, width) => FloatValue(value, width)
      case tungsten.NullValue => NullValue(IntType(8))
      case tungsten.ArrayValue(ety, elements) => {
        ArrayValue(convertType(ety), elements.map(convertValue(_, parent)))
      }
      case tungsten.StructValue(_, elements) => {
        StructValue(elements.map(convertValue(_, parent)))
      }
      case tungsten.DefinedValue(name, ty) => {
        val cTy = convertType(ty)
        module.getDefn(name) match {
          case Some(_: tungsten.Global) => DefinedValue(globalSymbol(name), cTy)
          case _ => DefinedValue(localSymbol(name, parent), cTy)
        }
      }
      case _ => throw new UnsupportedOperationException
    }
  }

  def convert32BitValue(value: tungsten.Value, parent: Option[Symbol]): Value = {
    value match {
      case tungsten.IntValue(v, _) => IntValue(v, 32)
      case tungsten.DefinedValue(name, _: tungsten.IntType) => {
        DefinedValue(localSymbol(name, parent), IntType(32))
      }
      case _ => convertValue(value, parent)
    }
  }

  def convertType(ty: tungsten.Type): Type = {
    ty match {
      case tungsten.UnitType => VoidType
      case tungsten.BooleanType => IntType(1)
      case tungsten.IntType(width) => IntType(width)
      case tungsten.FloatType(width) => FloatType(width)
      case tungsten.PointerType(tungsten.UnitType) => PointerType(IntType(8))
      case tungsten.PointerType(ety) => PointerType(convertType(ety))
      case tungsten.NullType => PointerType(IntType(8))
      case tungsten.ArrayType(size, ety) => ArrayType(size, convertType(ety))
      case tungsten.StructType(structName) => {
        val globalName = globalSymbol(structName)
        val localName = "%" + globalName.tail
        NamedStructType(localName)
      }
      case _ => throw new UnsupportedOperationException
    }
  }

  def globalSymbol(symbol: Symbol): String = {
    "@" + convertSymbol(symbol)
  }

  def localSymbol(symbol: Symbol, parent: Option[Symbol]): String = {
    def findChildName(name: List[String], parentName: List[String]): List[String] = {
      (name, parentName) match {
        case (nameHd :: nameTl, parentHd :: parentTl) if nameHd == parentHd => {
          findChildName(nameTl, parentTl)
        }
        case (Nil, _) => symbol.name
        case _ => name
      }
    }

    val parentName = parent.map(_.name).getOrElse(Nil)
    val childSymbol = new Symbol(findChildName(symbol.name, parentName), symbol.id)
    "%" + convertSymbol(childSymbol)
  }    

  def convertSymbol(symbol: Symbol): String = {
    def convertNamePart(part: String): String = {
      val buffer = new StringBuffer
      for (c <- part) {
        if (c.toInt < 0x80 && !Character.isISOControl(c) && c != '"' && c != '\\')
          buffer.append(c)
        else if (c.toInt < 0xFF)
          buffer.append("\\%02x".format(c.toInt))
        else
          buffer.append("\\%02x\\%02x".format(c.toInt >>> 8, c.toInt & 0xFF))
      }
      buffer.toString
    }
    
    val idStr = if (symbol.id != 0) "." + symbol.id else ""
    val nameStr = symbol.name.map(convertNamePart _).mkString(".")
    val symbolStr = nameStr + idStr
    val identRegex = "[a-zA-Z$._][a-zA-Z$._0-9]*".r
    identRegex.findFirstIn(symbolStr) match {
      case Some(m) if m == symbolStr => symbolStr
      case _ => "\"" + symbolStr + "\""
    }
  }    
}

object TungstenToLlvmConverter {
  def apply(module: tungsten.Module): Module = {
    new TungstenToLlvmConverter(module).convert
  }
}
