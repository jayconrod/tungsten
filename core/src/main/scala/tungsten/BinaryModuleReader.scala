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
import Utilities._

class BinaryModuleReader(input: DataInputStream) {
  val strings = new ArrayBuffer[String]
  val symbols = new ArrayBuffer[Symbol]

  def read: Module = {
    val header = readHeader
    readTable(strings, readString)
    readTable(symbols, readSymbol)
    val empty = new scala.collection.immutable.TreeMap[Symbol, Definition]()
    val definitions = (0 until symbols.size).foldLeft(empty) { (definitions, _) =>
      val defn = readDefinition
      if (definitions.contains(defn.name))
        throw new IOException("Duplicate definition")
      definitions + (defn.name -> defn)
    }
    new Module(header._1, header._2, header._3, header._4, header._5, header._6, header._7, header._8,
               definitions)
  }

  def readHeader = {
    val magic = input.readInt
    if (magic != MAGIC)
      throw new IOException("Invalid magic number")
    val formatVersion = (input.readByte, input.readByte)
    if (formatVersion != VERSION)
      throw new IOException("Invalid version")

    val name = readHeaderSymbol
    val ty = readModuleType
    val version = readVersion
    val filename = readOption(new File(readString))
    val dependencies = readList(readModuleDependency)
    val searchPaths = readList(new File(readString))
    val is64Bit = readBoolean
    val isSafe = readBoolean
    (name, ty, version, filename, dependencies, searchPaths, is64Bit, isSafe)
  }

  def readHeaderSymbol: Symbol = {
    val name = readList(input.readUTF)
    val id = readInt
    new Symbol(name, id)
  }

  def readModuleType: ModuleType = {
    import ModuleType._
    input.readByte match {
      case 0 => INTERMEDIATE
      case 1 => LIBRARY
      case 2 => PROGRAM
      case _ => throw new IOException("Invalid module type")
    }
  }

  def readModuleDependency: ModuleDependency = {
    val name = readHeaderSymbol
    val minVersion = readVersion
    val maxVersion = readVersion
    new ModuleDependency(name, minVersion, maxVersion)
  }

  def readTable[T](table: ArrayBuffer[T], reader: => T) {
    val size = readInt
    if (size < 0)
      throw new IOException("Invalid table size")
    for (i <- 0 until size)
      table += reader
  }

  def readDefinition: Definition = {
    val name = symbol
    val id = input.readByte
    id match {
      case ANNOTATION_ID => Annotation(name, readList(symbol), readAnnotations)
      case BLOCK_ID => Block(name, readList(symbol), readList(symbol), readAnnotations)
      case FIELD_ID => Field(name, readType, readAnnotations)
      case FUNCTION_ID => {
        Function(name, 
                 readType,          // return type
                 readList(symbol),  // type parameters
                 readList(symbol),  // parameters
                 readList(symbol),  // blocks
                 readAnnotations)
      }
      case GLOBAL_ID => Global(name, readType, readOption(readValue), readAnnotations)
      case PARAMETER_ID => Parameter(name, readType, readAnnotations)
      case STRUCT_ID => Struct(name, readList(symbol), readAnnotations)
      case CLASS_ID => {
        Class(name, 
              readList(symbol), 
              readOption(readCast[ClassType](readType)),
              readList(readCast[InterfaceType](readType)),
              readList(readList(symbol)),
              readList(symbol),
              readList(symbol),
              readList(symbol),
              readAnnotations)
      }
      case INTERFACE_ID => {
        Interface(name,
                  readList(symbol),
                  readCast[ClassType](readType),
                  readList(readCast[InterfaceType](readType)),
                  readList(readList(symbol)),
                  readList(symbol),
                  readAnnotations)
      }
      case TYPE_PARAMETER_ID => {
        TypeParameter(name, readOption(readType), readOption(readType), 
                      readVariance, readAnnotations)
      }
      case ADDRESS_INST_ID => {
        AddressInstruction(name, readType, readValue, readList(readValue), readAnnotations)
      }
      case BINARY_OPERATOR_INST_ID => {
        BinaryOperatorInstruction(name, readType, readBinaryOperator, 
                                  readValue, readValue, readAnnotations)
      }
      case BIT_CAST_INST_ID => {
        BitCastInstruction(name, readType, readValue, readAnnotations)
      }
      case BRANCH_INST_ID => {
        BranchInstruction(name, readType, symbol, readList(readValue), readAnnotations)
      }
      case CONDITIONAL_BRANCH_INST_ID => {
        ConditionalBranchInstruction(name, readType, readValue, symbol, readList(readValue),
                                     symbol, readList(readValue), readAnnotations)
      }
      case EXTRACT_INST_ID => {
        ExtractInstruction(name, readType, readValue, readList(readValue), readAnnotations)
      }
      case FLOAT_EXTEND_INST_ID => {
        FloatExtendInstruction(name, readType, readValue, readAnnotations)
      }
      case FLOAT_TO_INTEGER_INST_ID => {
        FloatToIntegerInstruction(name, readType, readValue, readAnnotations)
      }
      case FLOAT_TRUNCATE_INST_ID => {
        FloatTruncateInstruction(name, readType, readValue, readAnnotations)
      }
      case HEAP_ALLOCATE_INST_ID => {
        HeapAllocateInstruction(name, readType, readAnnotations)
      }
      case HEAP_ALLOCATE_ARRAY_INST_ID => {
        HeapAllocateArrayInstruction(name, readType, readValue, readAnnotations)
      }
      case INSERT_INST_ID => {
        InsertInstruction(name, readType, readValue, readValue, 
                          readList(readValue), readAnnotations)
      }
      case INTEGER_SIGN_EXTEND_INST_ID => {
        IntegerSignExtendInstruction(name, readType, readValue, readAnnotations)
      }
      case INTEGER_TO_FLOAT_INST_ID => {
        IntegerToFloatInstruction(name, readType, readValue, readAnnotations)
      }
      case INTEGER_TRUNCATE_INST_ID => {
        IntegerTruncateInstruction(name, readType, readValue, readAnnotations)
      }
      case INTEGER_ZERO_EXTEND_INST_ID => {
        IntegerZeroExtendInstruction(name, readType, readValue, readAnnotations)
      }
      case INTRINSIC_CALL_INST_ID => {
        IntrinsicCallInstruction(name, readType, readIntrinsic, 
                                 readList(readValue), readAnnotations)
      }
      case LOAD_INST_ID => LoadInstruction(name, readType, readValue, readAnnotations)
      case LOAD_ELEMENT_INST_ID => {
        LoadElementInstruction(name, readType, readValue, readList(readValue), readAnnotations)
      }
      case NEW_INST_ID => {
        NewInstruction(name, readType, symbol, 
                       readList(readType), readList(readValue), readAnnotations)
      }
      case POINTER_CALL_INST_ID => {
        PointerCallInstruction(name, readType, readValue,
                               readList(readType), readList(readValue), readAnnotations)
      }
      case RELATIONAL_OPERATOR_INST_ID => {
        RelationalOperatorInstruction(name, readType, readRelationalOperator, 
                                      readValue, readValue, readAnnotations)
      }
      case RETURN_INST_ID => ReturnInstruction(name, readType, readValue, readAnnotations)
      case STORE_INST_ID => {
        StoreInstruction(name, readType, readValue, readValue, readAnnotations)
      }
      case STORE_ELEMENT_INST_ID => {
        StoreElementInstruction(name, readType, readValue, 
                                readValue, readList(readValue), readAnnotations)
      }
      case STACK_ALLOCATE_INST_ID => StackAllocateInstruction(name, readType, readAnnotations)
      case STACK_ALLOCATE_ARRAY_INST_ID => {
        StackAllocateArrayInstruction(name, readType, readValue, readAnnotations)
      }
      case STATIC_CALL_INST_ID => {
        StaticCallInstruction(name, readType, symbol, 
                              readList(readType), readList(readValue), readAnnotations)
      }
      case THROW_INST_ID => ThrowInstruction(name, readType, readValue, readAnnotations)
      case UPCAST_INST_ID => UpcastInstruction(name, readType, readValue, readAnnotations)
      case VIRTUAL_CALL_INST_ID => {
        VirtualCallInstruction(name, readType, readValue, readInt, 
                               readList(readType), readList(readValue), readAnnotations)
      }
      case VIRTUAL_LOOKUP_INST_ID => {
        VirtualLookupInstruction(name, readType, readValue, readInt, readAnnotations)
      }
      case _ => throw new IOException("Invalid definition ID: " + id)
    }
  }

  def readAnnotations: List[AnnotationValue] = {
    readList(readAnnotationValue)
  }

  def readAnnotationValue: AnnotationValue = {
    AnnotationValue(symbol, readList(readValue))
  }

  def readType: Type = {
    input.readByte match {
      case UNIT_TYPE_ID => UnitType
      case BOOLEAN_TYPE_ID => BooleanType
      case CHAR_TYPE_ID => CharType
      case STRING_TYPE_ID => StringType
      case INT_TYPE_ID => {
        val width = input.readByte.asInstanceOf[Int]
        if (!List(8, 16, 32, 64).contains(width))
          throw new IOException("Invalid integer type")
        IntType(width)
      }
      case FLOAT_TYPE_ID => {
        val width = input.readByte
        if (!List(32, 64).contains(width))
          throw new IOException("Invalid float type")
        FloatType(width)
      }
      case POINTER_TYPE_ID => PointerType(readType)
      case NULL_TYPE_ID => NullType
      case ARRAY_TYPE_ID => {
        val length = readLong
        if (length < 0)
          throw new IOException("Invalid array size: " + length)
        ArrayType(length, readType)
      }
      case STRUCT_TYPE_ID => StructType(symbol)
      case FUNCTION_TYPE_ID => FunctionType(readType, readList(symbol), readList(readType))
      case CLASS_TYPE_ID => ClassType(symbol, readList(readType))
      case INTERFACE_TYPE_ID => InterfaceType(symbol, readList(readType))
      case VARIABLE_TYPE_ID => VariableType(symbol)
      case _ => throw new IOException("Invalid type ID")
    }
  }

  def readValue: Value = {
    input.readByte match {
      case UNIT_VALUE_ID => UnitValue
      case BOOLEAN_VALUE_ID => BooleanValue(readBoolean)
      case CHAR_VALUE_ID => CharValue(readChar)
      case STRING_VALUE_ID => StringValue(string)
      case INT8_VALUE_ID => IntValue(input.readByte, 8)
      case INT16_VALUE_ID => IntValue(input.readShort, 16)
      case INT32_VALUE_ID => IntValue(input.readInt, 32)
      case INT64_VALUE_ID => IntValue(input.readLong, 64)
      case FLOAT32_VALUE_ID => FloatValue(input.readFloat, 32)
      case FLOAT64_VALUE_ID => FloatValue(input.readDouble, 64)
      case NULL_VALUE_ID => NullValue
      case ARRAY_VALUE_ID => ArrayValue(readType, readList(readValue))
      case STRUCT_VALUE_ID => StructValue(symbol, readList(readValue))
      case DEFINED_VALUE_ID => DefinedValue(symbol, readType)
      case BIT_CAST_VALUE_ID => BitCastValue(readValue, readType)
      case _ => throw new IOException("Invalid value ID")
    }
  }

  def readVariance: Variance = {
    import Variance._
    input.readByte match {
      case COVARIANT_ID => COVARIANT
      case CONTRAVARIANT_ID => CONTRAVARIANT
      case INVARIANT_ID => INVARIANT
      case _ => throw new IOException("Invalid variance ID")
    }
  }

  def readVersion: Version = {
    val elements = readList(readInt)
    if (elements.exists(_ < 0))
      throw new IOException("Invalid version")
    new Version(elements)
  }

  def readSymbol: Symbol = {
    val name = readList(get(strings, readInt))
    val id = readInt
    Symbol(name, id)
  }

  def string = get(strings, readInt)

  def symbol = get(symbols, readInt)

  def get[T](table: ArrayBuffer[T], index: Int) = {
    if (!table.indices.contains(index))
      throw new IOException("Invalid table index")
    table(index)
  }

  def readString: String = input.readUTF

  def readInt: Int = input.readInt

  def readLong: Long = input.readLong

  def readChar: Char = input.readChar

  def readBoolean: Boolean = {
    input.readByte match {
      case 0 => false
      case 1 => true
      case _ => throw new IOException("Invalid Boolean value")
    }
  }

  def readList[T](reader: => T): List[T] = {
    val length = readInt
    if (length < 0)
      throw new IOException("Invalid list length")
    val buffer = new ArrayBuffer[T]
    for (i <- 0 until length)
      buffer += reader
    buffer.toList
  }

  def readOption[T](reader: => T): Option[T] = {
    input.readByte match {
      case 0 => None
      case 1 => Some(reader)
      case _ => throw new IOException("Invalid option ID")
    }
  }

  def readCast[S](reader: => AnyRef): S = {
    try {
      reader.asInstanceOf[S]
    } catch {
      case exn: ClassCastException =>
        throw new InvalidObjectException("object is not of the correct type")
    }
  }

  def readBinaryOperator: BinaryOperator = {
    import BinaryOperator._
    input.readByte match {
      case BINOP_MULTIPLY_ID => MULTIPLY
      case BINOP_DIVIDE_ID => DIVIDE
      case BINOP_REMAINDER_ID => REMAINDER
      case BINOP_ADD_ID => ADD
      case BINOP_SUBTRACT_ID => SUBTRACT
      case BINOP_LEFT_SHIFT_ID => LEFT_SHIFT
      case BINOP_RIGHT_SHIFT_ARITHMETIC_ID => RIGHT_SHIFT_ARITHMETIC
      case BINOP_RIGHT_SHIFT_LOGICAL_ID => RIGHT_SHIFT_LOGICAL
      case BINOP_AND_ID => AND
      case BINOP_XOR_ID => XOR
      case BINOP_OR_ID => OR
      case _ => throw new IOException("Invalid binary operator")
    }
  }

  def readRelationalOperator: RelationalOperator = {
    import RelationalOperator._
    input.readByte match {
      case RELOP_LESS_THAN_ID => LESS_THAN
      case RELOP_LESS_EQUAL_ID => LESS_EQUAL
      case RELOP_GREATER_THAN_ID => GREATER_THAN
      case RELOP_GREATER_EQUAL_ID => GREATER_EQUAL    
      case RELOP_EQUAL_ID => EQUAL
      case RELOP_NOT_EQUAL_ID => NOT_EQUAL
      case _ => throw new IOException("Invalid relational operator ID")
    }
  }

  def readIntrinsic: IntrinsicFunction = {
    import Intrinsic._
    val id = input.readShort
    id match {
      case INTRINSIC_EXIT_ID => EXIT
      case INTRINSIC_READ_ID => READ
      case INTRINSIC_WRITE_ID => WRITE
      case INTRINSIC_OPEN_ID => OPEN
      case INTRINSIC_CLOSE_ID => CLOSE
      case _ => throw new IOException("Invalid intrinsic ID: " + id)
    }
  }
}

