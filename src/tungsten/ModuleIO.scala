package tungsten

import java.io._
import scala.collection.mutable._
import scala.util.parsing.input.CharSequenceReader
import Utilities._

object ModuleIO {
  def readBinary(file: File): Module = {
    // TODO
    throw new UnsupportedOperationException
  }

  def readBinary(input: InputStream): Module = {
    // TODO
    throw new UnsupportedOperationException
  }

  def readText(file: File): Either[Module, List[CompileException]] = {
    val input = new BufferedReader(new FileReader(file))
    val module = readText(input, file.getName)
    input.close
    module
  }

  def readText(input: Reader): Either[Module, List[CompileException]] = {
    readText(input, "<STDIN>")
  }    

  def readText(input: Reader, filename: String): Either[Module, List[CompileException]] =
  {
    val text = readContentsOfFile(input)
    readText(text, filename)
  }

  def readText(text: String,
               filename: String = "<STDIN>"): Either[Module, List[CompileException]] =
  {
    parse(text, filename) match {
      case Right(error) => Right(List(ParseException(error, Nowhere)))
      case Left(ast) => {
        ast.compile match {
          case Right(errors) => Right(errors)
          case Left(module) => {
            val errors = module.validate
            if (!errors.isEmpty)
              Right(errors)
            else
              Left(module)
          }
        }
      }
    }
  }

  def writeBinary(module: Module, file: File) = {
    // TODO
    throw new UnsupportedOperationException
  }

  def writeBinary(module: Module, output: OutputStream) = {
    // TODO
    throw new UnsupportedOperationException
  }

  def writeText(module: Module, file: File) {
    val output = new BufferedWriter(new FileWriter(file))
    writeText(module, output)
    output.close
  }

  def writeText(module: Module, output: Writer) {
    // TODO
    throw new UnsupportedOperationException
  }

  /* readBinary helpers */

  /* readText helpers */
  def parse(text: String, filename: String): Either[AstModule, String] = {
    val reader = new CharSequenceReader(text)
    val scanner = new AstLexer.Scanner(filename, reader)
    AstParser.phrase(AstParser.module)(scanner) match {
      case AstParser.Success(ast, _) => Left(ast)
      case parseError: AstParser.NoSuccess => Right(parseError.msg)
    }
  }    

  /* writeBinary helpers */
  
  /* writeText helpers */
  val INDENT = "  "

  def isTopLevel(defn: Definition): Boolean = {
    defn match {
      case _: Global | _: Function | _: Struct => true
      case _ => false
    }
  }    

  def locationString(loc: Location): String = {
    if (loc == Nowhere)
      ""
    else
      loc.toString + " "
  }

  class TextModuleWriter(module: Module, output: Writer) {
    val parentNames = new Stack[Symbol]

    def writeDefinition(defn: Definition) {
      if (defn.isInstanceOf[Instruction])
        writeDefinition(defn.asInstanceOf[Instruction])
      else {
        try {
          val writer = getClass.getMethod("writeDefinition", defn.getClass)
          writer.invoke(this, defn)
        } catch {
          case exn => throw new RuntimeException(exn)
        }
      }
    }

    def writeDefinition(block: Block) {
      output.write(INDENT + "#block " + locationString(block.location) + block.name.simple)
      writeParameterList(block.parameters)
      output.write(" {\n")
      parentNames.push(block.name)
      writeDefinitionList(block.instructions, "")
      parentNames.pop
      output.write(INDENT + "}\n")
    }

    def writeDefinition(field: Field) {
      output.write(INDENT + "#field " + locationString(field.location) + field.name.simple + 
        ": " + field.ty + "\n")
    }

    def writeDefinition(function: Function) {
      output.write("#function " + locationString(function.location) + function.name)
      writeParameterList(function.parameters)
      output.write(": " + function.returnType + " {\n")
      writeDefinitionList(function.blocks, "")
      output.write("}\n")
    }

    def writeDefinition(global: Global) { 
      output.write("#global " + locationString(global.location) + global.name + 
        ": " + global.ty)
      global.value match {
        case Some(v) => output.write(" = " + v)
        case None => ()
      }
      output.write("\n")
    }

    def writeDefinition(param: Parameter) {
      val locStr = if (param.location == Nowhere)
        ""
      else
        " " + param.location + " "
      output.write(param.name.simple.toString + locStr + ": " + param.ty)
    }

    def writeDefinition(struct: Struct) {
      output.write("#struct " + locationString(struct.location) + struct.name + " {\n")
      writeDefinitionList(struct.fields, "")
      output.write("}\n")
    }

    def writeDefinition(inst: Instruction) {
      val localName = localSymbol(inst.name)
      def writeLocalValue(value: Value) {
        output.write(localValue(value).toString)
      }
      output.write(INDENT + INDENT)
      inst match {
        case AddressInstruction(name, base, indices, location) => {
          output.write("#address " + locationString(location) + localName + " = " + 
            localValue(base) + ", ")
          writeList(indices, writeLocalValue _, ", ")
        }
        case AssignInstruction(name, value, location) => {
          output.write("#assign " + locationString(location) + localName + 
            " = " + localValue(value))
        }
        case BinaryOperatorInstruction(name, operator, left, right, location) => {
          output.write("#binop " + locationString(location) + localName + " = " +
            localValue(left) + " " + operator.name + " " + localValue(right))
        }
        case BranchInstruction(name, target, arguments, location) => {
          output.write("#branch " + locationString(location) + localName + 
            " = " + target.simple)
          writeArgumentList(arguments)
        }
        case ConditionalBranchInstruction(name, condition, trueTarget, trueArgs,
                                          falseTarget, falseArgs, location) =>
        {
          output.write("#cond " + locationString(location) + localName + " = " +
            condition + " ? " + localSymbol(trueTarget))
          writeArgumentList(trueArgs)
          output.write(" : " + localSymbol(falseTarget))
          writeArgumentList(falseArgs)
        }
        case IndirectCallInstruction(name, target, arguments, location) => {
          output.write("#icall " + locationString(location) + localName + " = " + target)
          writeArgumentList(arguments)
        }
        case IntrinsicCallInstruction(name, intrinsic, arguments, location) => {
          output.write("#intrinsic " + locationString(location) + localName + 
            " = " + intrinsic.name)
          writeArgumentList(arguments)
        }
        case LoadInstruction(name, pointer, location) => {
          output.write("#load " + locationString(location) + localName + 
            " = *" + localValue(pointer))
        }
        case LoadElementInstruction(name, base, indices, location) => {
          output.write("#loadelement " + locationString(location) + localName + " = " +
            base + ", ")
          writeList(indices, writeLocalValue _, ", ")
        }
        case RelationalOperatorInstruction(name, operator, left, right, location) => {
          output.write("#relop " + locationString(location) + localName + " = " + 
            left + " " + operator.name + " " + right)
        }
        case ReturnInstruction(name, value, location) => {
          output.write("#return " + locationString(location) + localName + 
            " = " + localValue(value))
        }
        case StackAllocateInstruction(name, ty, location) => {
          output.write("#stack " + locationString(location) + localName + ": " + ty)
        }
        case StackAllocateArrayInstruction(name, count, elementType, location) => {
          output.write("#stackarray " + locationString(location) + localName + " = " +
            count + " * " + elementType)
        }
        case StaticCallInstruction(name, target, arguments, location) => {
          output.write("#scall " + locationString(location) + localName + " = " + target)
          writeArgumentList(arguments)
        }
        case StoreInstruction(name, pointer, value, location) => {
          output.write("#store " + locationString(location) + localName + " = *" + 
            localValue(pointer) + " <- " + localValue(value))
        }
        case StoreElementInstruction(name, base, indices, value, location) => {
          output.write("#storeelement " + locationString(location) + localName + " = " + 
            localValue(base) + ", ")
          writeList(indices, writeLocalValue _, ", ")
          output.write(" <- " + localValue(value))
        }
        case UpcastInstruction(name, value, ty, location) => {
          output.write("#upcast " + locationString(location) + localName + 
            " = " + localValue(value) + ": " + ty)
        }
      }
      output.write("\n")
    }   

    def writeParameterList(parameters: List[Symbol]) {
      output.write("(")
      if (parameters.isEmpty)
        output.write(" ")
      else
        writeDefinitionList(parameters, ", ")
      output.write(")")
    }

    def writeArgumentList(arguments: List[Value]) {
      output.write("(")
      if (arguments.isEmpty)
        output.write(" ")
      else
        writeList(arguments, { (v: Value) => output.write(localValue(v).toString) }, ", ")
      output.write(")")
    }

    def writeList[T](list: List[T], writer: T => Unit, sep: String) { 
      list match {
        case Nil => ()
        case h :: Nil => writer(h)
        case h :: t => {
          writer(h)
          output.write(sep)
          writeList(t, writer, sep)
        }
      }
    }

    def writeDefinitionList(list: List[Symbol], sep: String) { 
      val definitionList = list.map(module.definitions(_))
      writeList(definitionList, writeDefinition(_: Definition), sep)
    }

    def localSymbol(sym: Symbol): Symbol = {
      def localName(l: List[String], r: List[String]): List[String] = {
        (l, r) match {
          case (lh :: lt, rh :: (rt @ _ :: _)) if lh == rh => localName(lt, rt)
          case _ => r
        }
      }
    
      parentNames.headOption match {
        case Some(parentName) => {
          val lname = localName(parentName.name.toList, sym.name.toList)
          Symbol(lname, sym.id)
        }
        case None => sym
      }
    }

    def localValue(value: Value): Value = {
      value match {
        case DefinedValue(name, location) => DefinedValue(localSymbol(name), location)
        case ArrayValue(elementType, elements, location) => {
          val localElements = elements.map(localValue _)
          ArrayValue(elementType, localElements, location)
        }
        case StructValue(structName, fields, location) => {
          val localFields = fields.map(localValue _)
          StructValue(structName, localFields, location)
        }
        case _ => value
      }
    }
  }

  /* magic numbers */
  val MAGIC = 0x574F626A    // 'WObj' in big-endian

  val VERSION: (Byte, Byte) = (0, 1)

  val BLOCK_ID: Byte = 1
  val FIELD_ID: Byte = 2
  val FUNCTION_ID: Byte = 3
  val GLOBAL_ID: Byte = 4
  val PARAMETER_ID: Byte = 5
  val STRUCT_ID: Byte = 6

  val ADDRESS_INST_ID: Byte = 100
  val ASSIGN_INST_ID: Byte = 101
  val BINARY_OPERATOR_INST_ID: Byte = 102
  val BRANCH_INST_ID: Byte = 103
  val CONDITIONAL_BRANCH_INST_ID: Byte = 104
  val INDIRECT_CALL_INST_ID: Byte = 105
  val INTRINSIC_CALL_INST_ID: Byte = 106
  val LOAD_INST_ID: Byte = 107
  val LOAD_ELEMENT_INST_ID: Byte = 108
  val RELATIONAL_OPERATOR_INST_ID: Byte = 109
  val RETURN_INST_ID: Byte = 110
  val STORE_INST_ID: Byte = 111
  val STORE_ELEMENT_INST_ID: Byte = 112
  val STACK_ALLOCATE_INST_ID: Byte = 113
  val STACK_ALLOCATE_ARRAY_INST_ID: Byte = 114
  val STATIC_CALL_INST_ID: Byte = 115
  val UPCAST_INST_ID: Byte = 116

  val BINOP_MULTIPLY_ID: Byte = 1
  val BINOP_DIVIDE_ID: Byte = 2
  val BINOP_REMAINDER_ID: Byte = 3
  val BINOP_ADD_ID: Byte = 4
  val BINOP_SUBTRACT_ID: Byte = 5
  val BINOP_LEFT_SHIFT_ID: Byte = 6
  val BINOP_RIGHT_SHIFT_ARITHMETIC_ID: Byte = 7
  val BINOP_RIGHT_SHIFT_LOGICAL_ID: Byte = 8
  val BINOP_AND_ID: Byte = 9
  val BINOP_XOR_ID: Byte = 10
  val BINOP_OR_ID: Byte = 11

  val RELOP_LESS_THAN_ID: Byte = 20
  val RELOP_LESS_EQUAL_ID: Byte = 21
  val RELOP_GREATER_THAN_ID: Byte = 22
  val RELOP_GREATER_EQUAL_ID: Byte = 23
  val RELOP_EQUAL_ID: Byte = 24
  val RELOP_NOT_EQUAL_ID: Byte = 25

  val INTRINSIC_EXIT_ID: Byte = 30

  val UNIT_TYPE_ID = 1
  val BOOLEAN_TYPE_ID = 2
  val INT_TYPE_ID = 3
  val FLOAT_TYPE_ID = 4
  val POINTER_TYPE_ID = 5
  val NULL_TYPE_ID = 6
  val ARRAY_TYPE_ID = 7
  val STRUCT_TYPE_ID = 8

  val UNIT_VALUE_ID = 1
  val BOOLEAN_VALUE_ID = 2
  val INT8_VALUE_ID = 3
  val INT16_VALUE_ID = 4
  val INT32_VALUE_ID = 5
  val INT64_VALUE_ID = 6
  val FLOAT32_VALUE_ID = 7
  val FLOAT64_VALUE_ID = 8
  val NULL_VALUE_ID = 9
  val ARRAY_VALUE_ID = 10
  val STRUCT_VALUE_ID = 11
  val DEFINED_VALUE_ID = 12

  /* old code */
/*
  def readBinary(file: File): Module = {
    val input = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))
    val reader = new BinaryModuleReader(input)
    val module = reader.read
    input.close
    module
  }

  def writeBinary(module: Module, file: File) = {
    val output = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
    val writer = new BinaryModuleWriter(module, output)
    writer.write
    output.close
  }

  class BinaryModuleWriter(module: Module, output: DataOutputStream) {
    import BinaryModuleMagicNumbers._

    val stringTable = new ArrayBuffer[String]
    val stringIndices = new HashMap[String, Int]
    val locationTable = new ArrayBuffer[Location]
    val locationIndices = new HashMap[Location, Int]
    val symbolTable = new ArrayBuffer[Symbol]
    val symbolIndices = new HashMap[Symbol, Int]

    def write {
      collect
      writeHeader
      writeSeq(stringTable, writeString _)
      writeSeq(locationTable, writeLocation _)
      module.definitions.valuesIterable.foreach(writeDefinition _)
    }

    def collect {
      module.definitions.valuesIterable.foreach(collectDefinition _)
    }

    def collectDefinition(defn: Definition) {
      collectSymbol(defn.name)
      collectLocation(defn.location)
      defn match {
        case _: Block => ()
        case Field(_, ty, _) => collectType(ty)
        case Function(_, _, _, returnType, _, _) => collectType(returnType)
        case Global(_, ty, value, _) => {
          collectType(ty)
          value.foreach(collectValue _)
        }
        case Parameter(_, ty, _) => collectType(ty)
        case Struct(_, _, _) => ()

        case inst: Instruction => inst match {      // we should get a warning if we miss one
          case AddressInstruction(_, base, indices, _) => {
            collectValue(base)
            indices.foreach(collectValue _)
          }
          case AssignInstruction(_, value, _) => collectValue(value)
          case BinaryOperatorInstruction(_, _, left, right, _) => {
            collectValue(left)
            collectValue(right)
          }
          case BranchInstruction(_, _, arguments, _) => arguments.foreach(collectValue _)
          case ConditionalBranchInstruction(_, condition, _, trueArgs, _, falseArgs, _) => {
            collectValue(condition)
            trueArgs.foreach(collectValue _)
            falseArgs.foreach(collectValue _)
          }
          case IndirectCallInstruction(_, _, arguments, _) => arguments.foreach(collectValue _)
          case IntrinsicCallInstruction(_, _, arguments, _) => arguments.foreach(collectValue _)
          case LoadInstruction(_, pointer, _) => collectValue(pointer)
          case LoadElementInstruction(_, base, indices, _) => {
            collectValue(base)
            indices.foreach(collectValue _)
          }
          case RelationalOperatorInstruction(_, _, left, right, _) => {
            collectValue(left)
            collectValue(right)
          }
          case StoreInstruction(_, pointer, value, _) => {
            collectValue(pointer)
            collectValue(value)
          }
          case StoreElementInstruction(_, base, indices, value, _) => {
            collectValue(base)
            indices.foreach(collectValue _)
            collectValue(value)
          }
          case ReturnInstruction(_, value, _) => collectValue(value)
          case StackAllocateInstruction(_, ty, _) => collectType(ty)
          case StackAllocateArrayInstruction(_, count, elementType, _) => {
            collectValue(count)
            collectType(elementType)
          }
          case StaticCallInstruction(_, _, arguments, _) => arguments.foreach(collectValue _)
          case UpcastInstruction(_, value, ty, _) => {
            collectValue(value)
            collectType(ty)
          }
        }
      }
    }

    def collectType(ty: Type) {
      collectLocation(ty.location)
    }

    def collectValue(value: Value) {
      collectLocation(value.location)
    }

    def collectSymbol(sym: Symbol) {
      assert(!symbolIndices.contains(sym))
      symbolIndices += sym -> symbolTable.size
      symbolTable += sym
      sym.name.foreach(collectString(_))
    }

    def collectLocation(loc: Location) {
      if (!locationIndices.contains(loc)) {
        locationIndices += loc -> locationTable.size
        locationTable += loc
        collectString(loc.filename)
      }
    }

    def collectString(str: String) {
      if (!stringIndices.contains(str)) {
        stringIndices += str -> stringTable.size
        stringTable += str
      }
    }      

    def writeHeader {
      output.writeInt(MAGIC)
      output.writeByte(VERSION._1)
      output.writeByte(VERSION._2)
    }

    def writeString(s: String) {
      output.writeUTF(s)
    }

    def writeLocation(loc: Location) {
      output.writeInt(stringIndices(loc.filename))
      output.writeInt(loc.beginLine)
      output.writeInt(loc.beginColumn)
      output.writeInt(loc.endLine)
      output.writeInt(loc.endColumn)
    }

    def writeSymbol(sym: Symbol) {
      writeSeq(sym.name, { (n: String) => output.writeInt(stringIndices(n)) })
      output.writeInt(sym.id)
    }

    def writeDefinition(defn: Definition) {
      output.writeInt(symbolIndices(defn.name))
      defn match {
        case Block(_, parameters, instructions, _) => {
          output.writeByte(BLOCK_ID)
          writeSeq(parameters, { (n: Symbol) => output.writeInt(symbolIndices(n)) })
          writeSeq(instructions, { (n: Symbol) => output.writeInt(symbolIndices(n)) })
        }
        case Field(_, ty, _) => {
          output.writeByte(FIELD_ID)
          writeType(ty)
        }
        case Function(_, _, parameters, returnType, blocks, _) => {
          output.writeByte(FUNCTION_ID)
          writeSeq(parameters, writeSymbol _)
          writeType(returnType)
          writeSeq(blocks, writeSymbol _)
        }
        case Global(_, ty, value, _) => {
          output.writeByte(GLOBAL_ID)
          writeType(ty)
          writeOption(value, writeValue _)
        }
        case Parameter(_, ty, _) => {
          output.writeByte(PARAMETER_ID)
          writeType(ty)
        }
        case Struct(_, fields, _) => {
          output.writeByte(STRUCT_ID)
          writeSeq(fields, writeSymbol _)
        }

        case inst: Instruction => inst match {      // we should get a warning if we miss one
          case AddressInstruction(_, base, indices, _) => {
            output.writeByte(ADDRESS_INST_ID)
            writeValue(base)
            writeSeq(indices, writeValue _)
          }
          case AssignInstruction(_, value, _) => {
            output.writeByte(ASSIGN_INST_ID)
            writeValue(value)
          }
          case BinaryOperatorInstruction(_, operator, left, right, _) => {
            import BinaryOperator._
            output.writeByte(BINARY_OPERATOR_INST_ID)
            output.writeByte(operator match {
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
              case _ => throw new AssertionError("operator " + operator)
            })
            writeValue(left)
            writeValue(right)
          }
          case BranchInstruction(_, target, arguments, _) => {
            output.writeByte(BRANCH_INST_ID)
            output.writeInt(symbolIndices(target))
            writeSeq(arguments, writeValue _)
          }
          case ConditionalBranchInstruction(_, condition, trueTarget, trueArguments, 
                                            falseTarget, falseArguments, _) =>
          {
            output.writeByte(CONDITIONAL_BRANCH_INST_ID)
            writeValue(condition)
            output.writeInt(symbolIndices(trueTarget))
            writeSeq(trueArguments, writeValue _)
            output.writeInt(symbolIndices(falseTarget))
            writeSeq(falseArguments, writeValue _)
          }
          case IndirectCallInstruction(_, target, arguments, _) => {
            output.writeByte(INDIRECT_CALL_INST_ID)
            writeValue(target)
            writeSeq(arguments, writeValue _)
          }
          case IntrinsicCallInstruction(_, intrinsic, arguments, _) => {          
            import Intrinsic._
            output.writeByte(INTRINSIC_CALL_INST_ID)
            output.writeByte(intrinsic match {
              case EXIT => INTRINSIC_EXIT_ID
            })
            writeSeq(arguments, writeValue _)
          }
          case LoadInstruction(_, pointer, _) => {
            output.writeByte(LOAD_INST_ID)
            writeValue(pointer)
          }
          case LoadElementInstruction(_, base, indices, _) => {
            output.writeByte(LOAD_ELEMENT_INST_ID)
            writeValue(base)
            writeSeq(indices, writeValue _)
          }
          case RelationalOperatorInstruction(_, operator, left, right, _) => {
            import RelationalOperator._
            output.writeByte(RELATIONAL_OPERATOR_INST_ID)
            output.writeByte(operator match {
              case LESS_THAN => RELOP_LESS_THAN_ID
              case LESS_EQUAL => RELOP_LESS_EQUAL_ID
              case GREATER_THAN => RELOP_GREATER_THAN_ID
              case GREATER_EQUAL => RELOP_GREATER_EQUAL_ID
              case EQUAL => RELOP_EQUAL_ID
              case NOT_EQUAL => RELOP_NOT_EQUAL_ID
            })
            writeValue(left)
            writeValue(right)
          }            
          case ReturnInstruction(_, value, _) => {
            output.writeByte(RETURN_INST_ID)
            writeValue(value)
          }
          case StoreInstruction(_, pointer, value, _) => {
            output.writeByte(STORE_INST_ID)
            writeValue(pointer)
            writeValue(value)
          }
          case StoreElementInstruction(_, base, indices, value, _) => {
            output.writeByte(STORE_ELEMENT_INST_ID)
            writeValue(base)
            writeSeq(indices, writeValue _)
            writeValue(value)
          }
          case StackAllocateInstruction(_, ty, _) => {
            output.writeByte(STACK_ALLOCATE_INST_ID)
            writeType(ty)
          }
          case StackAllocateArrayInstruction(_, count, elementType, _) => {
            output.writeByte(STACK_ALLOCATE_ARRAY_INST_ID)
            writeValue(count)
            writeType(elementType)
          }
          case StaticCallInstruction(_, target, arguments, _) => {
            output.writeByte(STATIC_CALL_INST_ID)
            output.writeInt(symbolIndices(target))
            writeSeq(arguments, writeValue _)
          }
          case UpcastInstruction(_, value, ty, _) => {
            output.writeByte(UPCAST_INST_ID)
            writeValue(value)
            writeType(ty)
          }
        }
      }
      output.writeInt(locationIndices(defn.location))
    }

    def writeType(ty: Type) {
      ty match {
        case UnitType(_) => {
          output.writeByte(UNIT_TYPE_ID)
        }
        case BooleanType(_) => {
          output.writeByte(BOOLEAN_TYPE_ID)
        }
        case IntType(width, _) => {
          output.writeByte(INT_TYPE_ID)
          output.writeByte(width.asInstanceOf[Byte])
        }
        case FloatType(width, _) => {
          output.writeByte(FLOAT_TYPE_ID)
          output.writeByte(width.asInstanceOf[Byte])
        }
        case PointerType(elementType, _) => {
          output.writeByte(POINTER_TYPE_ID)
          writeType(elementType)
        }
        case NullType(_) => {
          output.writeByte(NULL_TYPE_ID)
        }
        case ArrayType(size, elementType, _) => {
          output.writeByte(ARRAY_TYPE_ID)
          writeOption(size, { (i: Int) => output.writeInt(i) })
          writeType(elementType)
        }
        case StructType(structName, _) => {
          output.writeByte(STRUCT_TYPE_ID)
          output.writeInt(symbolIndices(structName))
        }
      }
      output.writeInt(locationIndices(ty.location))
    }

    def writeValue(value: Value) {
      value match {
        case UnitValue(_) => {
          output.writeByte(UNIT_VALUE_ID)
        }
        case BooleanValue(v, _) => {
          output.writeByte(BOOLEAN_VALUE_ID)
          output.writeByte(if (v) 1 else 0)
        }
        case Int8Value(v, _) => {
          output.writeByte(INT8_VALUE_ID)
          output.writeByte(v)
        }
        case Int16Value(v, _) => {
          output.writeByte(INT16_VALUE_ID)
          output.writeShort(v)
        }
        case Int32Value(v, _) => {
          output.writeByte(INT32_VALUE_ID)
          output.writeInt(v)
        }
        case Int64Value(v, _) => {
          output.writeByte(INT64_VALUE_ID)
          output.writeLong(v)
        }
        case Float32Value(v, _) => {
          output.writeByte(FLOAT32_VALUE_ID)
          output.writeFloat(v)
        }
        case Float64Value(v, _) => {
          output.writeByte(FLOAT64_VALUE_ID)
          output.writeDouble(v)
        }
        case NullValue(_) => {
          output.writeByte(NULL_VALUE_ID)
        }
        case ArrayValue(elementType, elements, _) => {
          output.writeByte(ARRAY_VALUE_ID)
          writeType(elementType)
          writeSeq(elements, writeValue _)
        }
        case StructValue(structName, fields, _) => {
          output.writeByte(STRUCT_VALUE_ID)
          output.writeInt(symbolIndices(structName))
          writeSeq(fields, writeValue _)
        }
        case DefinedValue(sym, _) => {
          output.writeByte(DEFINED_VALUE_ID)
          output.writeInt(symbolIndices(sym))
        }
      }
      output.writeInt(locationIndices(value.location))
    }

    def writeSeq[T](seq: scala.collection.Seq[T], writeFn: T => Unit) {
      output.writeInt(seq.size)
      seq.foreach(writeFn)
    }

    def writeOption[T](opt: Option[T], writeFn: T => Unit) {
      opt match {
        case None => output.writeByte(0)
        case Some(e) => {
          output.writeByte(1)
          writeFn(e)
        }
      }
    }
  }

  class BinaryModuleReader(input: DataInputStream) {
    import BinaryModuleMagicNumbers._

    val stringTable = new ArrayBuffer[String]
    val locationTable = new ArrayBuffer[Location]
    val symbolTable = new ArrayBuffer[Symbol]

    def read: Module = {
      readHeader
      readStrings
      readLocations
      readSymbols
      val definitions = readDefinitions
      val empty = scala.collection.immutable.Map[Symbol, Definition]()
      val defnMap = definitions.foldLeft(empty) { (map, defn) =>
        map + (defn.name -> defn)
      }
      new Module(defnMap)
    }

    def readHeader {
      val magic = input.readInt
      if (magic != MAGIC)
        throw new IOException("Invalid magic number")
      val version = (input.readByte, input.readByte)
      if (magic != MAGIC || version != VERSION)
        throw new IOException("Invalid version")
    }

    def readStrings {
      val count = input.readInt
      if (count < 0)
        throw new IOException("Negative string count")
      for (i <- 0 until count)
        stringTable += readString
    }

    def readLocations {
      val count = input.readInt
      if (count < 0)
        throw new IOException("Negative location count")
      for (i <- 0 until count)
        locationTable += readLocation
    }

    def readSymbols {
      val count = input.readInt
      if (count < 0)
        throw new IOException("Negative symbol count")
      for (i <- 0 until count)
        symbolTable += readSymbol
    }

    def readDefinitions: List[Definition] = {
      val count = symbolTable.size
      val definitions = new ArrayBuffer[Definition](count)
      for (i <- 0 until count)
        definitions(i) = readDefinition
      definitions.toList
    }

    def readDefinition: Definition = {
      val name = getSymbol(input.readInt)
      def location = getLocation(input.readInt)

      input.readByte match {
        case BLOCK_ID => {
          val parameters = readList { () => getSymbol(input.readInt) }
          val instructions = readList { () => getSymbol(input.readInt) }
          Block(name, parameters, instructions, location)
        }
        case FIELD_ID => {
          val ty = readType
          Field(name, ty, location)
        }
        case FUNCTION_ID => {
          val parameters = readList { () => getSymbol(input.readInt) }
          val returnType = readType
          val blocks = readList { () => getSymbol(input.readInt) }
          Function(name, Nil, parameters, returnType, blocks, location)
        }
        case GLOBAL_ID => {
          val ty = readType
          val value = readOption(readValue _)
          Global(name, ty, value, location)
        }
        case PARAMETER_ID => {
          val ty = readType
          Parameter(name, ty, location)
        }
        case STRUCT_ID => {
          val fields = readList { () => getSymbol(input.readInt) }
          Struct(name, fields, location)
        }

        case ADDRESS_INST_ID => {
          val base = readValue
          val indices = readList(readValue _)
          AddressInstruction(name, base, indices, location)
        }
        case ASSIGN_INST_ID => {
          val value = readValue
          AssignInstruction(name, value, location)
        }
        case BINARY_OPERATOR_INST_ID => {
          val operator = readBinop
          val left = readValue
          val right = readValue
          BinaryOperatorInstruction(name, operator, left, right, location)
        }
        case BRANCH_INST_ID => {
          val target = getSymbol(input.readInt)
          val arguments = readList(readValue _)
          BranchInstruction(name, target, arguments, location)
        }
        case CONDITIONAL_BRANCH_INST_ID => {
          val condition = readValue
          val trueTarget = getSymbol(input.readInt)
          val trueArguments = readList(readValue _)
          val falseTarget = getSymbol(input.readInt)
          val falseArguments = readList(readValue _)
          ConditionalBranchInstruction(name, condition, trueTarget, trueArguments,
                                       falseTarget, falseArguments, location)
        }
        case INDIRECT_CALL_INST_ID => {
          val target = readValue
          val arguments = readList(readValue _)
          IndirectCallInstruction(name, target, arguments, location)
        }
        case INTRINSIC_CALL_INST_ID => {
          val intrinsic = readIntrinsic
          val arguments = readList(readValue _)
          IntrinsicCallInstruction(name, intrinsic, arguments, location)
        }
        case LOAD_INST_ID => {
          val pointer = readValue
          LoadInstruction(name, pointer, location)
        }
        case LOAD_ELEMENT_INST_ID => {
          val base = readValue
          val indices = readList(readValue _)
          LoadElementInstruction(name, base, indices, location)
        }
        case RELATIONAL_OPERATOR_INST_ID => {
          val relop = readRelop
          val left = readValue
          val right = readValue
          RelationalOperatorInstruction(name, relop, left, right, location)
        }
        case RETURN_INST_ID => {
          val value = readValue
          ReturnInstruction(name, value, location)
        }
        case STORE_INST_ID => {
          val pointer = readValue
          val value = readValue
          StoreInstruction(name, pointer, value, location)
        }
        case STORE_ELEMENT_INST_ID => {
          val base = readValue
          val indices = readList(readValue _)
          val value = readValue
          StoreElementInstruction(name, base, indices, value, location)
        }
        case STACK_ALLOCATE_INST_ID => {
          val ty = readType
          StackAllocateInstruction(name, ty, location)
        }
        case STACK_ALLOCATE_ARRAY_INST_ID => {
          val count = readValue
          val elementType = readType
          StackAllocateArrayInstruction(name, count, elementType, location)
        }
        case STATIC_CALL_INST_ID => {
          val target = getSymbol(input.readInt)
          val arguments = readList(readValue _)
          StaticCallInstruction(name, target, arguments, location)
        }
        case UPCAST_INST_ID => {
          val value = readValue
          val ty = readType
          UpcastInstruction(name, value, ty, location)
        }
        case _ => throw new IOException("Invalid definition ID")
      }
    }

    def readType: Type = {
      def location = getLocation(input.readInt)
      input.readByte match {
        case UNIT_TYPE_ID => UnitType(location)
        case INT_TYPE_ID => {
          val width = input.readByte
          if (!List(8, 16, 32, 64).contains(width))
            throw new IOException("Invalid integer width")
          IntType(width, location)
        }
        case FLOAT_TYPE_ID => {
          val width = input.readByte
          if (!List(32, 64).contains(width))
            throw new IOException("Invalid float width")
          FloatType(width, location)
        }
        case POINTER_TYPE_ID => PointerType(readType, location)
        case NULL_TYPE_ID => NullType(location)
        case ARRAY_TYPE_ID => {
          val size = readOption { () =>
            val sz = input.readInt
            if (sz < 0)
              throw new IOException("Invalid array type size")
            sz
          }
          ArrayType(size, readType, location)
        }
        case STRUCT_TYPE_ID => StructType(getSymbol(input.readInt), location)
        case _ => throw new IOException("Invalid type ID")
      }
    }

    def readValue: Value = {
      def location = getLocation(input.readInt)
      input.readByte match {
        case UNIT_VALUE_ID => UnitValue(location)
        case BOOLEAN_VALUE_ID => {
          val b = input.readByte
          if (b == 0)
            BooleanValue(false, location)
          else if (b == 1)
            BooleanValue(true, location)
          else
            throw new IOException("Invalid Boolean value")
        }
        case INT8_VALUE_ID => Int8Value(input.readByte, location)
        case INT16_VALUE_ID => Int16Value(input.readShort, location)
        case INT32_VALUE_ID => Int32Value(input.readInt, location)
        case INT64_VALUE_ID => Int64Value(input.readLong, location)
        case FLOAT32_VALUE_ID => Float32Value(input.readFloat, location)
        case FLOAT64_VALUE_ID => Float64Value(input.readDouble, location)
        case NULL_VALUE_ID => NullValue(location)
        case ARRAY_VALUE_ID => ArrayValue(readType, readList(readValue _), location)
        case STRUCT_VALUE_ID => {
          StructValue(getSymbol(input.readInt), readList(readValue _), location)
        }
        case DEFINED_VALUE_ID => DefinedValue(getSymbol(input.readInt), location)
        case _ => throw new IOException("Invalid value ID")
      }
    }

    def readBinop: BinaryOperator = {
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
        case _ => throw new IOException("Invalid binary operator ID")
      }
    }

    def readRelop: RelationalOperator = {
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
      input.readByte match {
        case INTRINSIC_EXIT_ID => EXIT
        case _ => throw new IOException("Invalid intrinsic ID")
      }
    }

    def readSymbol: Symbol = {
      val name = readList { () => getString(input.readInt) }
      val id = input.readInt
      if (id < 0)
        throw new IOException("Invalid symbol ID")
      new Symbol(name, id)
    }

    def readLocation: Location = {
      val filename = getString(input.readInt)
      val beginLine = input.readInt
      val beginColumn = input.readInt
      val endLine = input.readInt
      val endColumn = input.readInt
      if (filename.isEmpty ||
          beginLine < 1 || beginColumn < 1 || endLine < 1 || endColumn < 1 ||
          (beginLine > endLine) || (beginLine == endLine && beginColumn > endColumn))
      {
        throw new IOException("Invalid location")
      }
      new Location(filename, beginLine, beginColumn, endLine, endColumn)
    }

    def readString: String = {
      input.readUTF
    }

    def readList[T](readFunction: () => T): List[T] = {
      val count = input.readInt
      if (count < 0)
        throw new IOException("Invalid list size")
      val result = for (i <- 0 until count)
        yield readFunction()
      result.toList
    }

    def readOption[T](readFunction: () => T): Option[T] = {
      val opt = input.readByte
      if (opt == 0)
        None
      else if (opt == 1)
        Some(readFunction())
      else
        throw new IOException("Invalid option ID")
    }

    def getSymbol(index: Int): Symbol = {
      if (symbolTable.isDefinedAt(index))
        symbolTable(index)
      else
        throw new IOException("Invalid symbol index")
    }

    def getLocation(index: Int): Location = {
      if (locationTable.isDefinedAt(index))
        locationTable(index)
      else
        throw new IOException("Invalid location index")
    }

    def getString(index: Int): String = {
      if (stringTable.isDefinedAt(index))
        stringTable(index)
      else
        throw new IOException("Invalid string index")
    }    
  }
*/
}
