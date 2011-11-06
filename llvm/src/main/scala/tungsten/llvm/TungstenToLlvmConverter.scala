/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten.llvm

import tungsten.Symbol
import tungsten.Utilities._

class TungstenToLlvmConverter(module: tungsten.Module) {
  def convert(targetTriple: Option[String] = None): Module = {
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
    new Module(dataLayout, targetTriple, cDefinitions)
  }

  def convertGlobal(global: tungsten.Global): Global = {
    val cName = globalSymbol(global.name)
    val cValue = global.value match {
      case Some(v) => Left(convertValue(v, None))
      case None => Right(convertType(global.ty))
    }
    Global(cName, cValue)
  }

  def convertStruct(struct: tungsten.Struct): Struct = {
    val cName = structSymbol(struct.name)
    val fields = module.getFields(struct.fields)
    val cFieldTypes = fields.map { f: tungsten.Field => convertType(f.ty) }
    Struct(cName, cFieldTypes)
  }

  def convertFunction(function: tungsten.Function): Function = {
    val cName = globalSymbol(function.name)
    val cRetAttribs = convertParameterAttributes(function.annotations)
    val cReturnType = convertType(function.returnType)
    val parameters = module.getParameters(function.parameters)
    val isVariadic = function.isVariadic(module)
    val cParameters = if (isVariadic)
      parameters.dropRight(1).map(convertParameter(_, Some(function.name)))
    else
      parameters.map(convertParameter(_, Some(function.name)))
    val blocks = module.getBlocks(function.blocks)
    val cBlocks = blocks.map(convertBlock(_, Some(function.name)))
    val noReturnName = symbolFromString("tungsten.NoReturn")
    val cFnAttribs = convertFunctionAttributes(function.annotations)
    Function(cName, cRetAttribs, cReturnType, cParameters, isVariadic, cFnAttribs, cBlocks)
  }

  def convertParameter(parameter: tungsten.Parameter, parent: Option[Symbol]): Parameter = {
    val cName = localSymbol(parameter.name, parent)
    val cType = convertType(parameter.ty)
    val cAttribs = convertParameterAttributes(parameter.annotations)
    Parameter(cName, cType, cAttribs)
  }

  def convertBlock(block: tungsten.Block, parent: Option[Symbol]): Block = {
    assert(block.parameters.isEmpty)
    val cName = localSymbol(block.name, parent)
    val instructions = module.getInstructions(block.instructions)
    assert(instructions.last.isTerminating)
    val cInstructions = instructions.takeRight(2) match {
      // Special case: calls which require error handling will always appear at the end of
      // the block (LlvmCompatibilityPass does this). We translate them into invokes.
      case (callInst: tungsten.CallInstruction) ::
           (branchInst: tungsten.BranchInstruction) :: Nil 
           if block.catchBlock.isDefined => 
      {
        val cRest = instructions.take(instructions.size - 2).map(convertInstruction(_, parent))
        val catchBlockName = block.catchBlock.get._1

        val (target, arguments) = callInst match {
          case scallInst: tungsten.StaticCallInstruction => {
            val functionTy = module.getFunction(scallInst.target).ty(module)
            val target = tungsten.DefinedValue(scallInst.target, functionTy)
            (target, scallInst.arguments)
          }
          case pcallInst: tungsten.PointerCallInstruction =>
            (pcallInst.target, pcallInst.arguments)
          case _ => throw new RuntimeException("invalid call instruction")
        }            
        val cName = localSymbol(callInst.name, parent)
        val cTy = convertType(callInst.ty)
        val cTarget = convertValue(target, parent)
        val cArguments = arguments.map(convertValue(_, parent))
        val normalName = localSymbol(branchInst.target, parent)
        val cNormalLabel = DefinedValue(normalName, LabelType)
        val unwindName = localSymbol(catchBlockName, parent)
        val cUnwindLabel = DefinedValue(unwindName, LabelType)
        val cInvoke = InvokeInstruction(cName, None, Set(), cTy, None,
                                        cTarget, cArguments, Set(),
                                        cNormalLabel, cUnwindLabel)

        cRest :+ cInvoke
      }

      case _ => instructions.map(convertInstruction(_, parent))
    }
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
                                     indices.map(convertValue(_, parent)))
      }
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
      case tungsten.ExtractInstruction(_, _, base, indices, _) => {
        ExtractValueInstruction(localName, convertValue(base, parent), 
                                indices.map(convertValue(_, parent)))
      }
      case tungsten.FloatExtendInstruction(_, ty, value, _) =>
        FloatExtendInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.FloatToIntegerInstruction(_, ty, value, _) =>
        FloatToSignedIntegerInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.FloatTruncateInstruction(_, ty, value, _) =>
        FloatTruncateInstruction(localName, convertValue(value, parent), convertType(ty))
      case tungsten.InsertInstruction(_, _, value, base, indices, _) => {
        InsertValueInstruction(localName, 
                               convertValue(base, parent),
                               convertValue(value, parent),
                               indices.map(convertValue(_, parent)))
      }
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
      case tungsten.PointerCallInstruction(_, ty, target, _, arguments, _) => {
        val cReturnType = convertType(ty)
        val cTarget = convertValue(target, parent)
        val cArguments = arguments.map(convertValue(_, parent))
        CallInstruction(localName, false, None, Set(), 
                        cReturnType, None,
                        cTarget, cArguments, Set())
      }
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
      case tungsten.StackAllocateInstruction(_, ty, _) => {
        val tungsten.PointerType(elementType, _) = ty
        AllocaInstruction(localName, convertType(elementType))
      }
      case tungsten.StackAllocateArrayInstruction(_, ty, count, _) => {
        val tungsten.PointerType(elementType, _) = ty
        AllocaArrayInstruction(localName, convertType(elementType), convertValue(count, parent))
      }        
      case tungsten.StaticCallInstruction(_, ty, target, _, arguments, _) => {
        val function = module.getFunction(target)
        val functionType = function.ty(module)
        val cFunctionType = convertFunctionType(functionType)
        val cRetAttribs = convertParameterAttributes(function.annotations)
        val cReturnType = cFunctionType.returnType
        val cTargetType = if (cFunctionType.isVariadic) Some(PointerType(cFunctionType)) else None
        val cArgumentTypes = arguments.map { a: tungsten.Value => convertType(a.ty) }
        val cFnAttribs = convertFunctionAttributes(function.annotations)
        CallInstruction(localName, false, None, cRetAttribs, 
                        cReturnType, cTargetType,
                        DefinedValue(globalSymbol(target), cFunctionType), 
                        arguments.map(convertValue(_, parent)), cFnAttribs)
      }
      case tungsten.StoreInstruction(_, _, value, address, _) =>
        StoreInstruction(convertValue(value, parent), convertValue(address, parent), None)
      case tungsten.UnreachableInstruction(_, _, _) =>
        UnreachableInstruction
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
      case _ => {
        System.err.println(instruction.getClass.toString)
        throw new UnsupportedOperationException // TODO
      }
    }
  }

  def convertFunctionAttributes(annotations: List[tungsten.AnnotationValue]): Set[FunctionAttribute] = {
    import tungsten.{AnnotationValue => AV}
    import FunctionAttribute._
    val noreturn = symbolFromString("tungsten.NoReturn")
    (Set[FunctionAttribute]() /: annotations) { (attribs, ann) =>
      ann match {
        case AV(noreturn, _) => attribs + NORETURN
        case _ => attribs
      }
    }
  }

  def convertParameterAttributes(annotations: List[tungsten.AnnotationValue]): Set[ParameterAttribute] = {
    // TODO
    Set()
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
      case tungsten.StructValue(name, elements) => {
        NamedStructValue(structSymbol(name), elements.map(convertValue(_, parent)))
      }
      case tungsten.DefinedValue(name, ty) => {
        val cTy = convertType(ty)
        module.getDefn(name) match {
          case Some(_: tungsten.Global | _: tungsten.Function) => DefinedValue(globalSymbol(name), cTy)
          case _ => DefinedValue(localSymbol(name, parent), cTy)
        }
      }
      case tungsten.BitCastValue(value, ty) => {
        val cValue = convertValue(value, parent)
        val cTy = convertType(ty)
        BitCastValue(cValue, cTy)
      }
      case _ => throw new UnsupportedOperationException("cannot convert un-lowered value: " + value)
    }
  }

  def convertFunctionType(ty: tungsten.FunctionType): FunctionType = {
    val cReturnType = convertType(ty.returnType)
    if (ty.isVariadic) {
      val cParamTypes = ty.parameterTypes.dropRight(1).map(convertType _)
      FunctionType(cReturnType, cParamTypes, true)
    } else {
      val cParamTypes = ty.parameterTypes.map(convertType _)
      FunctionType(cReturnType, cParamTypes, false)
    }
  }    

  def convertType(ty: tungsten.Type): Type = {
    ty match {
      case tungsten.UnitType => VoidType
      case tungsten.BooleanType => IntType(1)
      case tungsten.IntType(width) => IntType(width)
      case tungsten.FloatType(width) => FloatType(width)
      case tungsten.PointerType(tungsten.UnitType, _) => PointerType(IntType(8))
      case tungsten.PointerType(ety, _) => PointerType(convertType(ety))
      case tungsten.NullType => PointerType(IntType(8))
      case tungsten.ArrayType(size, ety) => ArrayType(size, convertType(ety))
      case tungsten.StructType(structName) => {
        val globalName = globalSymbol(structName)
        val localName = "%" + globalName.tail
        NamedStructType(localName)
      }
      case fty: tungsten.FunctionType => PointerType(convertFunctionType(fty))
      case _ => throw new UnsupportedOperationException(ty.getClass.toString)
    }
  }

  def globalSymbol(symbol: Symbol): String = {
    "@" + convertSymbol(symbol)
  }

  def structSymbol(symbol: Symbol): String = {
    '%' + convertSymbol(symbol)
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
  def apply(module: tungsten.Module, targetTriple: Option[String] = None): Module = {
    new TungstenToLlvmConverter(module).convert(targetTriple)
  }
}
