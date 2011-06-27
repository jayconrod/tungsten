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

package tungsten

import scala.collection.mutable._
import java.io._
import BinaryModuleIDs._

class BinaryModuleWriter(module: Module, output: DataOutputStream) {
  class Table[T] {
    private val values = new ArrayBuffer[T]
    private val indices = new HashMap[T, Int]

    def apply(value: T): Int = indices(value)
    def add(value: T) {
      if (!indices.contains(value)) {
        indices += (value -> values.size)
        values += value
      }
    }
    def get(index: Int): T = values(index)

    def write(writer: T => Unit) {
      writeInt(values.size)
      values.foreach(writer)
    }

    override def toString = values.mkString("[", ", ", "]")
  }

  val strings = new Table[String]
  val symbols = new Table[Symbol]

  def write {
    collect
    writeHeader
    strings.write(writeString _)
    symbols.write(writeSymbol _)
    module.definitions.values.foreach(writeDefinition _)
  }

  /** Fills in the strings and symbols tables by scanning every definition for strings and
   *  symbols. We assume that the input module is validated, so that the set of definition
   *  names is the full set of symbols used anywhere in the module.
   */
  def collect {
    module.definitions.values.foreach(collectDefinition _)
  }

  def collectDefinition(defn: Definition) {
    collectSymbol(defn.name)
    defn.annotations.flatMap(_.values).foreach(collectValue _)
    defn match {
      case Global(_, _, value, _) => value.foreach(collectValue _)
      case inst: Instruction => inst.operands.foreach(collectValue _)
      case _ => ()
    }
  }

  def collectValue(value: Value) {
    value match {
      case StringValue(s) => collectString(s)
      case ArrayValue(_, elements) => elements.foreach(collectValue _)
      case StructValue(_, fields) => fields.foreach(collectValue _)
      case _ => ()
    }
  }

  def collectSymbol(symbol: Symbol) {
    symbol.name.foreach(strings.add(_))
    symbols.add(symbol)
  }

  def collectString(string: String) {
    strings.add(string)
  }

  def writeHeader {
    output.writeInt(MAGIC)
    output.writeByte(VERSION._1)
    output.writeByte(VERSION._2)

    writeHeaderSymbol(module.name)
    writeModuleType
    writeVersion(module.version)
    writeOption(module.filename, { (file: File) => writeString(file.toString) })
    writeList(module.dependencies, writeModuleDependency _)
    writeList(module.searchPaths.map(_.toString), writeString _)
    writeBoolean(module.is64Bit)
    writeBoolean(module.isSafe)
  }

  def writeHeaderSymbol(sym: Symbol) {
    writeList(sym.name, output.writeUTF _)
    writeInt(sym.id)
  }

  def writeModuleType {
    import ModuleType._
    val typeId = module.ty match {
      case INTERMEDIATE => 0
      case LIBRARY => 1
      case PROGRAM => 2
    }
    output.writeByte(typeId)
  }

  def writeVersion(version: Version) {
    writeList(version.elements, writeInt _)
  }

  def writeModuleDependency(dep: ModuleDependency) {
    writeHeaderSymbol(dep.name)
    writeVersion(dep.minVersion)
    writeVersion(dep.maxVersion)
  }

  def writeDefinition(defn: Definition) {
    writeInt(symbols(defn.name))
    defn match {
      case Annotation(_, parameters, _) => {
        output.writeByte(ANNOTATION_ID)
        writeSymbolList(parameters)
      }
      case Block(_, parameters, instructions, _) => {
        output.writeByte(BLOCK_ID)
        writeSymbolList(parameters)
        writeSymbolList(instructions)
      }
      case Class(_, typeParameters, superclass, interfaceTypes, interfaceMethods, 
                 constructors, methods, fields, _) =>
      {
        output.writeByte(CLASS_ID)
        writeSymbolList(typeParameters)
        writeOption(superclass, writeType _)
        writeList(interfaceTypes, writeType _)
        writeList(interfaceMethods, writeSymbolList _)
        writeSymbolList(constructors)
        writeSymbolList(methods)
        writeSymbolList(fields)
      }
      case Field(_, ty, _) => {
        output.writeByte(FIELD_ID)
        writeType(ty)
      }
      case Function(_, returnType, typeParameters, parameters, blocks, _) => {
        output.writeByte(FUNCTION_ID)
        writeType(returnType)
        writeSymbolList(typeParameters)
        writeSymbolList(parameters)
        writeSymbolList(blocks)
      }          
      case Global(_, ty, value, _) => {
        output.writeByte(GLOBAL_ID)
        writeType(ty)
        writeOption(value, writeValue _)
      }
      case Interface(_, typeParameters, superclass, 
                     interfaceTypes, interfaceMethods, methods, _) =>
      {
        output.writeByte(INTERFACE_ID)
        writeSymbolList(typeParameters)
        writeType(superclass)
        writeList(interfaceTypes, writeType _)
        writeList(interfaceMethods, writeSymbolList _)
        writeSymbolList(methods)
      }
      case Parameter(_, ty, _) => {
        output.writeByte(PARAMETER_ID)
        writeType(ty)
      }
      case Struct(_, fields, _) => {
        output.writeByte(STRUCT_ID)
        writeSymbolList(fields)
      }
      case TypeParameter(_, upperBound, lowerBound, variance, _) => {
        output.writeByte(TYPE_PARAMETER_ID)
        writeOption(upperBound, writeType _)
        writeOption(lowerBound, writeType _)
        writeVariance(variance)
      }

      case inst: Instruction => {
        val instId = inst match {
          case _: AddressInstruction => ADDRESS_INST_ID
          case _: BinaryOperatorInstruction => BINARY_OPERATOR_INST_ID
          case _: BitCastInstruction => BIT_CAST_INST_ID
          case _: BranchInstruction => BRANCH_INST_ID
          case _: ConditionalBranchInstruction => CONDITIONAL_BRANCH_INST_ID
          case _: ExtractInstruction => EXTRACT_INST_ID
          case _: FloatExtendInstruction => FLOAT_EXTEND_INST_ID
          case _: FloatToIntegerInstruction => FLOAT_TO_INTEGER_INST_ID
          case _: FloatTruncateInstruction => FLOAT_TRUNCATE_INST_ID
          case _: HeapAllocateInstruction => HEAP_ALLOCATE_INST_ID
          case _: HeapAllocateArrayInstruction => HEAP_ALLOCATE_ARRAY_INST_ID
          case _: InsertInstruction => INSERT_INST_ID
          case _: IntegerToFloatInstruction => INTEGER_TO_FLOAT_INST_ID
          case _: IntegerSignExtendInstruction => INTEGER_SIGN_EXTEND_INST_ID
          case _: IntegerTruncateInstruction => INTEGER_TRUNCATE_INST_ID
          case _: IntegerZeroExtendInstruction => INTEGER_ZERO_EXTEND_INST_ID
          case _: IntrinsicCallInstruction => INTRINSIC_CALL_INST_ID
          case _: LoadInstruction => LOAD_INST_ID
          case _: LoadElementInstruction => LOAD_ELEMENT_INST_ID
          case _: NewInstruction => NEW_INST_ID
          case _: PointerCallInstruction => POINTER_CALL_INST_ID
          case _: RelationalOperatorInstruction => RELATIONAL_OPERATOR_INST_ID
          case _: ReturnInstruction => RETURN_INST_ID
          case _: StoreInstruction => STORE_INST_ID
          case _: StoreElementInstruction => STORE_ELEMENT_INST_ID
          case _: StackAllocateInstruction => STACK_ALLOCATE_INST_ID
          case _: StackAllocateArrayInstruction => STACK_ALLOCATE_ARRAY_INST_ID
          case _: StaticCallInstruction => STATIC_CALL_INST_ID
          case _: ThrowInstruction => THROW_INST_ID
          case _: UpcastInstruction => UPCAST_INST_ID
          case _: VirtualCallInstruction => VIRTUAL_CALL_INST_ID
          case _: VirtualLookupInstruction => VIRTUAL_LOOKUP_INST_ID
        }
        output.writeByte(instId)
        writeType(inst.ty)

        inst match {
          case AddressInstruction(_, _, base, indices, _) => {
            writeValue(base)
            writeList(indices, writeValue _)
          }
          case BinaryOperatorInstruction(_, _, operator, left, right, _) => {
            writeBinaryOperator(operator)
            writeValue(left)
            writeValue(right)
          }
          case BitCastInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case BranchInstruction(_, _, target, arguments, _) => {
            writeInt(symbols(target))
            writeList(arguments, writeValue _)
          }
          case ConditionalBranchInstruction(_, _, condition, trueTarget, trueArgs,
                                            falseTarget, falseArgs, _) =>
          {
            writeValue(condition)
            writeInt(symbols(trueTarget))
            writeList(trueArgs, writeValue _)
            writeInt(symbols(falseTarget))
            writeList(falseArgs, writeValue _)
          }
          case ExtractInstruction(_, _, value, indices, _) => {
            writeValue(value)
            writeList(indices, writeValue _)
          }
          case FloatExtendInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case FloatToIntegerInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case FloatTruncateInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case HeapAllocateInstruction(_, ty, _) => ()
          case HeapAllocateArrayInstruction(_, _, count, _) => {
            writeValue(count)
          }
          case InsertInstruction(_, _, value, base, indices, _) => {
            writeValue(value)
            writeValue(base)
            writeList(indices, writeValue _)
          }
          case IntegerSignExtendInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case IntegerToFloatInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case IntegerTruncateInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case IntegerZeroExtendInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case IntrinsicCallInstruction(_, _, intrinsic, arguments, _) => {
            writeIntrinsic(intrinsic)
            writeList(arguments, writeValue _)
          }
          case LoadInstruction(_, _, pointer, _) => {
            writeValue(pointer)
          }
          case LoadElementInstruction(_, _, base, indices, _) => {
            writeValue(base)
            writeList(indices, writeValue _)
          }
          case NewInstruction(_, _, constructor, typeArguments, arguments, _) => {
            writeInt(symbols(constructor))
            writeList(typeArguments, writeType _)
            writeList(arguments, writeValue _)
          }
          case PointerCallInstruction(_, _, target, typeArguments, arguments, _) => {
            writeValue(target)
            writeList(typeArguments, writeType _)
            writeList(arguments, writeValue _)
          }
          case RelationalOperatorInstruction(_, _, operator, left, right, _) => {
            writeRelationalOperator(operator)
            writeValue(left)
            writeValue(right)
          }
          case ReturnInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case StoreInstruction(_, _, value, pointer, _) => {
            writeValue(value)
            writeValue(pointer)
          }
          case StoreElementInstruction(_, _, value, base, indices, _) => {
            writeValue(value)
            writeValue(base)
            writeList(indices, writeValue _)
          }
          case _: StackAllocateInstruction => ()
          case StackAllocateArrayInstruction(_, _, count, _) => {
            writeValue(count)
          }
          case StaticCallInstruction(_, _, target, typeArguments, arguments, _) => {
            writeInt(symbols(target))
            writeList(typeArguments, writeType _)
            writeList(arguments, writeValue _)
          }
          case ThrowInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case UpcastInstruction(_, _, value, _) => {
            writeValue(value)
          }
          case VirtualCallInstruction(_, _, target, methodIndex, typeArguments, arguments, _) => {
            writeValue(target)
            writeInt(methodIndex)
            writeList(typeArguments, writeType _)
            writeList(arguments, writeValue _)
          }
          case VirtualLookupInstruction(_, _, obj, methodIndex, _) => {
            writeValue(obj)
            writeInt(methodIndex)
          }
        }
      }
    }
    writeList(defn.annotations, writeAnnotationValue _)
  }

  def writeAnnotationValue(av: AnnotationValue) {
    writeInt(symbols(av.name))
    writeList(av.values, writeValue _)
  }

  def writeType(ty: Type) {
    ty match {
      case UnitType => output.writeByte(UNIT_TYPE_ID)
      case BooleanType => output.writeByte(BOOLEAN_TYPE_ID)
      case CharType => output.writeByte(CHAR_TYPE_ID)
      case StringType => output.writeByte(STRING_TYPE_ID)
      case IntType(width) => {
        output.writeByte(INT_TYPE_ID)
        output.writeByte(width)
      }
      case FloatType(width) => {
        output.writeByte(FLOAT_TYPE_ID)
        output.writeByte(width)
      }
      case PointerType(elementType) => {
        output.writeByte(POINTER_TYPE_ID)
        writeType(elementType)
      }
      case NullType => output.writeByte(NULL_TYPE_ID)
      case ArrayType(length, elementType) => {
        output.writeByte(ARRAY_TYPE_ID)
        writeLong(length)
        writeType(elementType)
      }
      case StructType(structName) => {
        output.writeByte(STRUCT_TYPE_ID)
        writeInt(symbols(structName))
      }
      case FunctionType(returnType, typeParameters, parameterTypes) => {
        output.writeByte(FUNCTION_TYPE_ID)
        writeType(returnType)
        writeSymbolList(typeParameters)
        writeList(parameterTypes, writeType _)
      }
      case ClassType(className, typeParameters) => {
        output.writeByte(CLASS_TYPE_ID)
        writeInt(symbols(className))
        writeList(typeParameters, writeType _)
      }
      case InterfaceType(interfaceName, typeParameters) => {
        output.writeByte(INTERFACE_TYPE_ID)
        writeInt(symbols(interfaceName))
        writeList(typeParameters, writeType _)
      }
      case VariableType(variableName) => {
        output.writeByte(VARIABLE_TYPE_ID)
        writeInt(symbols(variableName))
      }
    }
  }

  def writeValue(value: Value) {
    value match {
      case UnitValue => output.writeByte(UNIT_VALUE_ID)
      case BooleanValue(b) => {
        output.writeByte(BOOLEAN_VALUE_ID)
        writeBoolean(b)
      }
      case CharValue(ch) => {
        output.writeByte(CHAR_VALUE_ID)
        writeChar(ch)
      }
      case StringValue(s) => {
        output.writeByte(STRING_VALUE_ID)
        writeInt(strings(s))
      }
      case IntValue(v, 8) => {
        output.writeByte(INT8_VALUE_ID)
        output.writeByte(v.toByte)
      }
      case IntValue(v, 16) => {
        output.writeByte(INT16_VALUE_ID)
        output.writeShort(v.toShort)
      }
      case IntValue(v, 32) => {
        output.writeByte(INT32_VALUE_ID)
        output.writeInt(v.toInt)
      }
      case IntValue(v, _) => {
        output.writeByte(INT64_VALUE_ID)
        output.writeLong(v)
      }
      case FloatValue(v, 32) => {
        output.writeByte(FLOAT32_VALUE_ID)
        output.writeFloat(v.toFloat)
      }
      case FloatValue(v, _) => {
        output.writeByte(FLOAT64_VALUE_ID)
        output.writeDouble(v)
      }
      case NullValue => output.writeByte(NULL_VALUE_ID)
      case ArrayValue(elementType, elements) => {
        output.writeByte(ARRAY_VALUE_ID)
        writeType(elementType)
        writeList(elements, writeValue _)
      }
      case StructValue(structName, fields) => {
        output.writeByte(STRUCT_VALUE_ID)
        writeInt(symbols(structName))
        writeList(fields, writeValue _)
      }
      case DefinedValue(name, ty) => {
        output.writeByte(DEFINED_VALUE_ID)
        writeInt(symbols(name))
        writeType(ty)
      }
      case BitCastValue(value, ty) => {
        output.writeByte(BIT_CAST_VALUE_ID)
        writeValue(value)
        writeType(ty)
      }
    }
  }

  def writeBinaryOperator(operator: BinaryOperator) = {
    import BinaryOperator._
    val id = operator match {
      case MULTIPLY => BINOP_MULTIPLY_ID
      case DIVIDE => BINOP_DIVIDE_ID
      case REMAINDER => BINOP_REMAINDER_ID
      case ADD => BINOP_ADD_ID
      case SUBTRACT => BINOP_SUBTRACT_ID
      case LEFT_SHIFT => BINOP_LEFT_SHIFT_ID
      case RIGHT_SHIFT_ARITHMETIC => BINOP_RIGHT_SHIFT_ARITHMETIC_ID
      case RIGHT_SHIFT_LOGICAL => BINOP_RIGHT_SHIFT_LOGICAL_ID
      case AND => BINOP_AND_ID
      case XOR => BINOP_XOR_ID
      case OR => BINOP_OR_ID
    }
    output.writeByte(id)
  }

  def writeRelationalOperator(operator: RelationalOperator) = {
    import RelationalOperator._
    val id = operator match {
      case LESS_THAN => RELOP_LESS_THAN_ID
      case LESS_EQUAL => RELOP_LESS_EQUAL_ID
      case GREATER_THAN => RELOP_GREATER_THAN_ID
      case GREATER_EQUAL => RELOP_GREATER_EQUAL_ID
      case EQUAL => RELOP_EQUAL_ID
      case NOT_EQUAL => RELOP_NOT_EQUAL_ID
    }
    output.writeByte(id)
  }

  def writeIntrinsic(intrinsic: IntrinsicFunction) = {
    import Intrinsic._
    val id = intrinsic match {
      case EXIT => INTRINSIC_EXIT_ID
      case READ => INTRINSIC_READ_ID
      case WRITE => INTRINSIC_WRITE_ID
      case OPEN => INTRINSIC_OPEN_ID
      case CLOSE => INTRINSIC_CLOSE_ID
    }
    output.writeShort(id)
  }

  def writeVariance(variance: Variance) = {
    import Variance._
    val id = variance match {
      case COVARIANT => COVARIANT_ID
      case CONTRAVARIANT => CONTRAVARIANT_ID
      case INVARIANT => INVARIANT_ID
    }
    output.writeByte(id)
  }

  def writeSymbol(symbol: Symbol) {
    writeList(symbol.name, { (n: String) => writeInt(strings(n)) })
    writeInt(symbol.id)
  }

  def writeString(str: String) {
    output.writeUTF(str)
  }

  def writeInt(n: Int) {
    output.writeInt(n)
  }

  def writeLong(n: Long) {
    output.writeLong(n)
  }

  def writeChar(ch: Char) {
    output.writeChar(ch)
  }

  def writeBoolean(b: Boolean) {
    output.writeByte(if (b) 1 else 0)
  }

  def writeList[T](list: List[T], writer: T => Unit) {
    writeInt(list.size)
    list.foreach(writer)
  }

  def writeSymbolList(list: List[Symbol]) {
    writeList(list, { (sym: Symbol) => writeInt(symbols(sym)) })
  }

  def writeOption[T](opt: Option[T], writer: T => Unit) {
    opt match {
      case Some(e) => {
        output.writeByte(1)
        writer(e)
      }
      case None => output.writeByte(0)
    }
  }
}

