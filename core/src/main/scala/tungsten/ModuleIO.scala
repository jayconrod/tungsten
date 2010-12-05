package tungsten

import java.io._
import scala.collection.mutable._
import scala.util.parsing.input.CharSequenceReader
import Utilities._

object ModuleIO {
  def readBinary(file: File): Module = {
    var input: InputStream = null
    try {
      input = new BufferedInputStream(new FileInputStream(file))
      readBinary(input)
    } finally {
      if (input != null)
        input.close
    }
  }

  def readBinary(input: InputStream): Module = {
    val in = new DataInputStream(input)
    val reader = new BinaryModuleReader(in)
    reader.read
  }

  def readText(file: File): Module = {
    val input = new BufferedReader(new FileReader(file))
    val module = readText(input, file.getName)
    input.close
    module
  }

  def readText(input: Reader): Module = {
    readText(input, "<STDIN>")
  }    

  def readText(input: Reader, filename: String): Module =
  {
    val text = readContentsOfFile(input)
    readText(text, filename)
  }

  def readText(text: String,
               filename: String = "<STDIN>"): Module =
  {
    parse(text, filename) match {
      case Left(module) => module
      case Right(errors) => throw new IOException(errors.head)
    }
  }

  def writeBinary(module: Module, file: File) {
    val output = new BufferedOutputStream(new FileOutputStream(file))
    writeBinary(module, output)
    output.close
  }

  def writeBinary(module: Module, output: OutputStream) {
    val out = new DataOutputStream(output)
    val writer = new BinaryModuleWriter(module, out)
    writer.write
    out.flush
  }

  def writeText(module: Module, file: File) {
    val output = new BufferedWriter(new FileWriter(file))
    writeText(module, output)
    output.close
  }

  def writeText(module: Module, output: Writer) {
    val writer = new TextModuleWriter(module, output)
    writer.write
    output.flush
  }

  /* readBinary helpers */
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
      input.readByte match {
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
          StaticCallInstruction(name, readType, symbol, readList(readValue), readAnnotations)
        }
        case UPCAST_INST_ID => UpcastInstruction(name, readType, readValue, readAnnotations)
        case _ => throw new IOException("Invalid definition ID")
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
      input.readByte match {
        case INTRINSIC_EXIT_ID => EXIT
        case _ => throw new IOException("Invalid intrinsic ID")
      }
    }
  }

  /* readText helpers */
  def parse(text: String, filename: String): Either[Module, List[CompileException]] = {
    val file = new File(filename)
    val reader = new CharSequenceReader(text)
    val scanner = new Lexer.Scanner(reader)
    val parser = new Parser
    parser.phrase(parser.module(file))(scanner) match {
      case parser.Success((headers, asts), _) => {
        val definitions = asts.map(_.toList).flatten
        var module = headers
        var errors: List[CompileException] = Nil
        for (defn <- definitions) {
          if (module.definitions.contains(defn.name))
            errors ::= RedefinedSymbolException(defn.name, defn.getLocation, module.definitions(defn.name).getLocation)
          else
            module = module.add(defn)
        }
        if (errors.isEmpty)
          Left(module)
        else
          Right(errors)
      }
      case error: parser.NoSuccess => Right(List(ParseException(error.msg, Nowhere)))
    }
  }            

  /* writeBinary helpers */
  
  /* writeText helpers */
  class TextModuleWriter(module: Module, output: Writer) {
    val INDENT = "  "

    def write {
      writeHeader
      for (defn <- module.definitions.values) {
        defn match {
          case a: Annotation => writeAnnotation(a)
          case c: Class => writeClass(c)
          case g: Global => writeGlobal(g)
          case f: Function => writeFunction(f)
          case i: Interface => writeInterface(i)
          case s: Struct => writeStruct(s)
          case _ => ()
        }
        val isGlobal = defn match {
          case _: Annotation | _: Class | _: Global | _: Interface | _: Function | _: Struct => 
            true
          case _ => false
        }
        if (isGlobal)
          output.write("\n\n")
      }
    }

    def writeHeader {
      output.write("name: @" + module.name + "\n")
      val tyStr = module.ty match {
        case ModuleType.INTERMEDIATE => "intermediate"
        case ModuleType.LIBRARY => "library"
        case ModuleType.PROGRAM => "program"
      }
      output.write("type: " + tyStr + "\n")
      if (module.version != Version.MIN)
        output.write("version: " + module.version + "\n")
      module.filename match {
        case Some(filename) => output.write("filename: \"" + filename + "\"\n")
        case _ => ()
      }
      if (!module.dependencies.isEmpty)
        output.write("dependencies: " + module.dependencies.mkString(", ") + "\n")
      if (!module.searchPaths.isEmpty)
        output.write("searchpaths: " + module.searchPaths.mkString(", ") + "\n")
      output.write("is64bit: " + module.is64Bit + "\n")
      if (module.isSafe)
        output.write("safe: true\n")
      output.write("\n")
    }

    def writeAnnotation(annotation: Annotation) {
      writeAnnotations(annotation.annotations)
      output.write("annotation ")
      writeSymbol(annotation.name, None)
      writeFields(annotation.parameters, Some(annotation.name))
    }

    def writeClass(clas: Class) {
      writeAnnotations(clas.annotations)
      output.write("class ")
      writeSymbol(clas.name, None)
      writeTypeParameters(clas.typeParameters, Some(clas.name))
      clas.superclass.foreach { case t =>
        output.write(" <: ")
        writeType(t, None)
      }
      output.write(" {\n")

      writeImplementedInterfaces(clas.interfaceTypes, clas.interfaceMethods, Some(clas.name))

      output.write(INDENT + "constructors")
      writeSymbolBlock(clas.constructors, Some(clas.name))
      output.write("\n")

      output.write(INDENT + "methods")
      writeSymbolBlock(clas.methods, Some(clas.name))
      output.write("\n")

      val fields = module.getFields(clas.fields)
      writeChildren(fields, writeField(_: Field, Some(clas.name)), "", "\n", "\n")

      output.write("}")
    }

    def writeFunction(function: Function) {
      writeAnnotations(function.annotations)
      output.write("function ")
      writeType(function.returnType, Some(function.name))
      output.write(" ")
      writeSymbol(function.name, None)
      writeTypeParameters(function.typeParameters, Some(function.name))
      writeParameters(function.parameters, Some(function.name))
      writeBlocks(function.blocks, Some(function.name))
    }

    def writeGlobal(global: Global) {
      writeAnnotations(global.annotations)
      output.write("global ")
      writeType(global.ty, None)
      output.write(" ")
      writeSymbol(global.name, None)
      global.value match {
        case Some(v) => {
          output.write(" = ")
          writeValue(v, None)
        }
        case None => ()
      }
    }

    def writeInterface(interface: Interface) {
      writeAnnotations(interface.annotations)
      output.write("interface ")
      writeSymbol(interface.name, None)
      writeTypeParameters(interface.typeParameters, Some(interface.name))
      output.write(" <: ")
      writeType(interface.supertype, None)
      output.write(" {\n")

      writeImplementedInterfaces(interface.interfaceTypes, 
                                 interface.interfaceMethods, 
                                 Some(interface.name))

      output.write(INDENT + "methods")
      writeSymbolBlock(interface.methods, Some(interface.name))
      output.write("\n}")      
    }

    def writeStruct(struct: Struct) {
      writeAnnotations(struct.annotations)
      output.write("struct @" + struct.name)
      writeFields(struct.fields, Some(struct.name))
    }

    def writeFields(fields: List[Symbol], parentName: Option[Symbol]) {
      writeChildren(module.getFields(fields), writeField(_: Field, parentName),
                    " {\n", "\n", "\n}")
    }

    def writeParameters(parameters: List[Symbol], parentName: Option[Symbol]) {
      writeChildren(module.getParameters(parameters), writeParameter(_: Parameter, parentName),
                    "(", ", ", ")")
    }

    def writeTypeParameters(typeParameters: List[Symbol], parentName: Option[Symbol]) {
      writeChildren(module.getTypeParameters(typeParameters), 
                    writeTypeParameter(_: TypeParameter, parentName),
                    "[", ", ", "]")
    }

    def writeImplementedInterfaces(interfaceTypes: List[InterfaceType],
                                   interfaceMethods: List[List[Symbol]],
                                   parent: Option[Symbol])
    {
      (interfaceTypes zip interfaceMethods).map { case (ty, methods) =>
        output.write(INDENT)
        writeType(ty, parent)
        writeSymbolBlock(methods, parent)
        output.write("\n")
      }
    }

    def writeBlocks(blocks: List[Symbol], parentName: Option[Symbol]) {
      writeChildren(module.getBlocks(blocks), writeBlock(_: Block, parentName),
                    " {\n", "\n", "\n}")
    }

    def writeInstructions(instructions: List[Symbol], parentName: Option[Symbol]) {
      writeChildren(module.getInstructions(instructions), 
                    writeInstruction(_: Instruction, parentName),
                    " {\n", "\n", "\n" + INDENT + "}")
    }

    def writeBlock(block: Block, parentName: Option[Symbol]) {
      output.write(INDENT)
      writeAnnotations(block.annotations)
      output.write("block ")
      writeSymbol(block.name, parentName)
      writeParameters(block.parameters, parentName)
      writeInstructions(block.instructions, Some(block.name))
    }

    def writeField(field: Field, parentName: Option[Symbol]) {
      output.write(INDENT)
      writeAnnotations(field.annotations)
      output.write("field ")
      writeType(field.ty, parentName)
      output.write(" ")
      writeSymbol(field.name, parentName)
    }

    def writeParameter(parameter: Parameter, parentName: Option[Symbol]) {
      writeAnnotations(parameter.annotations)
      writeType(parameter.ty, parentName)
      output.write(" ")
      writeSymbol(parameter.name, parentName)
    }

    def writeTypeParameter(typeParameter: TypeParameter, parentName: Option[Symbol]) {
      writeAnnotations(typeParameter.annotations)
      output.write("type " + typeParameter.variance)
      writeSymbol(typeParameter.name, parentName)
      typeParameter.upperBound.foreach { t =>
        output.write(" <: " + localType(t, parentName))
      }
      typeParameter.lowerBound.foreach { t =>
        output.write(" >: " + localType(t, parentName))
      }
    }

    def writeInstruction(instruction: Instruction, parentName: Option[Symbol]) {
      val localName = this.localSymbol(instruction.name, parentName)
      def localSymbol(symbol: Symbol): String = this.localSymbol(symbol, parentName).toString
      def localValue(value: Value): String = this.localValue(value, parentName)
      def localType(ty: Type): String = this.localType(ty, parentName)

      val instName = instruction match {
        case _: AddressInstruction => "address"
        case _: BinaryOperatorInstruction => "binop"
        case _: BitCastInstruction => "bitcast"
        case _: BranchInstruction => "branch"
        case _: ConditionalBranchInstruction => "cond"
        case _: ExtractInstruction => "extract"
        case _: FloatExtendInstruction => "fextend"
        case _: FloatToIntegerInstruction => "ftoi"
        case _: FloatTruncateInstruction => "ftruncate"
        case _: HeapAllocateInstruction => "heap"
        case _: HeapAllocateArrayInstruction => "heaparray"
        case _: InsertInstruction => "insert"
        case _: IntegerToFloatInstruction => "itof"
        case _: IntegerSignExtendInstruction => "isextend"
        case _: IntegerTruncateInstruction => "itruncate"
        case _: IntegerZeroExtendInstruction => "izextend"
        case _: IntrinsicCallInstruction => "intrinsic"
        case _: LoadInstruction => "load"
        case _: LoadElementInstruction => "loadelement"
        case _: RelationalOperatorInstruction => "relop"
        case _: ReturnInstruction => "return"
        case _: StoreInstruction => "store"
        case _: StoreElementInstruction => "storeelement"
        case _: StackAllocateInstruction => "stack"
        case _: StackAllocateArrayInstruction => "stackarray"
        case _: StaticCallInstruction => "scall"
        case _: UpcastInstruction => "upcast"
      }

      output.write(INDENT + INDENT)
      writeAnnotations(instruction.annotations)
      output.write(instName + " " + localType(instruction.ty) + " " + localName)
      instruction match {
        case _: HeapAllocateInstruction | _: StackAllocateInstruction => ()
        case _ => output.write(" = ")
      }
      instruction match {
        case AddressInstruction(_, _, base, indices, _) => {
          output.write(localValue(base) + ", " + indices.map(localValue _).mkString(", "))
        }
        case BinaryOperatorInstruction(_, _, operator, left, right, _) => {
          output.write(localValue(left) + " " + operator.name + " " + localValue(right))
        }
        case BitCastInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case BranchInstruction(_, _, target, arguments, _) => {
          output.write(localSymbol(target))
          writeArguments(arguments, parentName)
        }
        case ConditionalBranchInstruction(_, _, condition, trueTarget, trueArgs,
                                          falseTarget, falseArgs, _) =>
        {
          output.write(localValue(condition) + " ? " + localSymbol(trueTarget))
          writeArguments(trueArgs, parentName)
          output.write(" : " + localSymbol(falseTarget))
          writeArguments(falseArgs, parentName)
        }
        case ExtractInstruction(_, _, base, indices, anns) => {
          output.write(localValue(base) + ", " + indices.map(localValue).mkString(", "))
        }
        case FloatExtendInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case FloatToIntegerInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case FloatTruncateInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case HeapAllocateInstruction(_, ty, _) => ()
        case HeapAllocateArrayInstruction(_, _, count, _) => {
          output.write(localValue(count))
        }
        case InsertInstruction(_, _, value, base, indices, _) => {
          output.write(localValue(value) + ", " + localValue(base) + ", " +
                       indices.map(localValue).mkString(", "))
        }
        case IntegerSignExtendInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case IntegerToFloatInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case IntegerTruncateInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case IntegerZeroExtendInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case IntrinsicCallInstruction(_, _, intrinsic, arguments, _) => {
          output.write(intrinsic.name)
          writeArguments(arguments, parentName)
        }
        case LoadInstruction(_, _, pointer, _) => {
          output.write(localValue(pointer))
        }
        case LoadElementInstruction(_, _, base, indices, _) => {
          output.write(base + ", " + indices.map(localValue _).mkString(", "))
        }
        case RelationalOperatorInstruction(_, _, operator, left, right, _) => {
          output.write(localValue(left) + " " + operator.name + " " + localValue(right))
        }
        case ReturnInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
        case StackAllocateInstruction(_, ty, _) => ()
        case StackAllocateArrayInstruction(_, _, count, _) => {
          output.write(localValue(count))
        }
        case StaticCallInstruction(_, _, target, arguments, _) => {
          output.write(localSymbol(target))
          writeArguments(arguments, parentName)
        }
        case StoreInstruction(_, _, value, pointer, _) => {
          output.write(localValue(value) + ", " + localValue(pointer))
        }
        case StoreElementInstruction(_, _, value, base, indices, _) => {
          output.write(localValue(value) + ", " + localValue(base) + ", " + 
            indices.map(localValue _).mkString(", "))
        }
        case UpcastInstruction(_, _, value, _) => {
          output.write(localValue(value))
        }
      }
    }

    def writeAnnotations(annotations: List[AnnotationValue]) {
      writeChildren(annotations, writeAnnotationValue _, "", " ", " ")
    }

    def writeAnnotationValue(annotation: AnnotationValue) {
      writeSymbol(annotation.name, None)
      if (!annotation.values.isEmpty)
        writeArguments(annotation.values, None)
    }

    def writeArguments(values: List[Value], parentName: Option[Symbol]) {
      output.write(values.map(localValue(_, parentName)).mkString("(", ", ", ")"))
    }

    def writeSymbolBlock(symbols: List[Symbol], parent: Option[Symbol]) {
      writeChildren(symbols, writeSymbol(_: Symbol, parent),
                    " {\n" + INDENT * 2,
                    ",\n" + INDENT * 2,
                    "\n" + INDENT + "}")
    }

    def writeChildren[T](children: List[T],
                         writer: T => Unit,
                         prefix: String,
                         separator: String,
                         suffix: String)
    {
      if (children.isEmpty)
        return

      output.write(prefix)
      for ((child, i) <- children.zip(0 until children.size)) {
        writer(child)
        if (i < children.size - 1)
          output.write(separator)
      }
      output.write(suffix)
    }

    def writeValue(value: Value, parentName: Option[Symbol]) {
      output.write(localValue(value, parentName))
    }

    def writeType(ty: Type, parentName: Option[Symbol]) {
      output.write(localType(ty, parentName))
    }

    def writeSymbol(symbol: Symbol, parentName: Option[Symbol]) {
      output.write(localSymbol(symbol, parentName).toString)
    }

    def localSymbol(symbol: Symbol, parentName: Option[Symbol]): String = {
      val simpleName = symbol.name.last
      parentName match {
        case Some(parent) if parent.name :+ simpleName == symbol.name => {
          "%" + Symbol(simpleName, symbol.id).toString
        }
        case _ => "@" + symbol.toString
      }
    }

    def localValue(value: Value, parentName: Option[Symbol]): String = {
      value match {
        case UnitValue => "()"
        case BooleanValue(true) => "true"
        case BooleanValue(false) => "false"
        case v @ CharValue(c) => {
          if (charIsPrintable(c) && c != '\'' && c != '\\')
            "'" + c + "'"
          else
            "'\\%04x'".format(c.toInt)
        }
        case StringValue(s) => {
          val buffer = new StringBuffer
          buffer.append('"')
          for (c <- s) {
            if (charIsPrintable(c) && c != '"' && c != '\\')
              buffer.append(c)
            else
              buffer.append("\\%04x".format(c.toInt))
          }
          buffer.append('"')
          buffer.toString
        }
        case IntValue(v, w) => "int%d %d".format(w, v)
        case FloatValue(v, w) => "float%d %f".format(w, v)
        case NullValue => "null"
        case ArrayValue(elementType, elements) => {
          "[%d x %s] {%s}".format(elements.size, localType(elementType, parentName), 
                                  elements.map(localValue(_: Value, parentName)).mkString(", "))
        }
        case StructValue(structName, elements) => {
          "struct %s {%s}".format(localSymbol(structName, parentName), 
                                  elements.map(localValue(_: Value, parentName)).mkString(", "))
        }
        case DefinedValue(name, ty) => {
          localType(ty, parentName) + " " + localSymbol(name, parentName)
        }
      }
    }

    def localType(ty: Type, parentName: Option[Symbol]): String = {
      def localTypeArguments(tyArgs: List[Type]): String = {
        if (tyArgs.isEmpty)
          ""
        else
          tyArgs.map(localType(_, parentName)).mkString("[", ", ", "]")
      }
      ty match {
        case UnitType => "unit"
        case BooleanType => "boolean"
        case CharType => "char"
        case StringType => "string"
        case IntType(w) => "int%d".format(w)
        case FloatType(w) => "float%d".format(w)
        case PointerType(ety) => localType(ety, parentName) + "*"
        case NullType => "nulltype"
        case ArrayType(length, elementType) => {
          "[%d x %s]".format(length, localType(elementType, parentName))
        }
        case StructType(structName) => "struct " + localSymbol(structName, parentName)
        case FunctionType(returnType, typeParameters, parameterTypes) => {
          val typeParameterStr = if (typeParameters.isEmpty)
            ""
          else
            typeParameters.map(localSymbol(_, parentName)).mkString("[", ", ", "]")
          val parameterStr = parameterTypes.map(localType(_: Type, parentName))
          val returnTypeStr = localType(returnType, parentName)
          "%s(%s) => %s".format(typeParameterStr, parameterStr, returnTypeStr)
        }
        case ClassType(className, tyArgs) => {
          "class " + localSymbol(className, parentName) + localTypeArguments(tyArgs)
        }
        case InterfaceType(interfaceName, tyArgs) => {
          "interface " + localSymbol(interfaceName, parentName) + localTypeArguments(tyArgs)
        }
        case VariableType(variableName) => "type " + localSymbol(variableName, parentName)
      }
    }
  }

  /* writeBinary helpers */
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
            case _: RelationalOperatorInstruction => RELATIONAL_OPERATOR_INST_ID
            case _: ReturnInstruction => RETURN_INST_ID
            case _: StoreInstruction => STORE_INST_ID
            case _: StoreElementInstruction => STORE_ELEMENT_INST_ID
            case _: StackAllocateInstruction => STACK_ALLOCATE_INST_ID
            case _: StackAllocateArrayInstruction => STACK_ALLOCATE_ARRAY_INST_ID
            case _: StaticCallInstruction => STATIC_CALL_INST_ID
            case _: UpcastInstruction => UPCAST_INST_ID
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
            case StaticCallInstruction(_, _, target, arguments, _) => {
              writeInt(symbols(target))
              writeList(arguments, writeValue _)
            }
            case UpcastInstruction(_, _, value, _) => {
              writeValue(value)
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
      }
      output.writeByte(id)
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

  /* magic numbers */
  val MAGIC = 0x574F626A    // 'WObj' in big-endian

  val VERSION: (Byte, Byte) = (0, 3)

  val ANNOTATION_ID: Byte = 1
  val BLOCK_ID: Byte = 2
  val FIELD_ID: Byte = 3
  val FUNCTION_ID: Byte = 4
  val GLOBAL_ID: Byte = 5
  val PARAMETER_ID: Byte = 6
  val STRUCT_ID: Byte = 7
  val CLASS_ID: Byte = 8
  val INTERFACE_ID: Byte = 9
  val TYPE_PARAMETER_ID: Byte = 10

  val ADDRESS_INST_ID: Byte = 100
  // 101 free
  val BINARY_OPERATOR_INST_ID: Byte = 102
  val BIT_CAST_INST_ID: Byte = 103
  val BRANCH_INST_ID: Byte = 104
  val CONDITIONAL_BRANCH_INST_ID: Byte = 105
  val EXTRACT_INST_ID: Byte = 106
  val INSERT_INST_ID: Byte = 107
  val INTRINSIC_CALL_INST_ID: Byte = 108
  val LOAD_INST_ID: Byte = 109
  val LOAD_ELEMENT_INST_ID: Byte = 110
  val RELATIONAL_OPERATOR_INST_ID: Byte = 111
  val RETURN_INST_ID: Byte = 112
  val STORE_INST_ID: Byte = 113
  val STORE_ELEMENT_INST_ID: Byte = 114
  val STACK_ALLOCATE_INST_ID: Byte = 115
  val STACK_ALLOCATE_ARRAY_INST_ID: Byte = 116
  val STATIC_CALL_INST_ID: Byte = 117
  val UPCAST_INST_ID: Byte = 118
  val HEAP_ALLOCATE_INST_ID = 119
  val HEAP_ALLOCATE_ARRAY_INST_ID = 120
  val FLOAT_EXTEND_INST_ID = 121
  val FLOAT_TO_INTEGER_INST_ID = 122
  val FLOAT_TRUNCATE_INST_ID = 123
  val INTEGER_SIGN_EXTEND_INST_ID = 124
  val INTEGER_TO_FLOAT_INST_ID = 125
  val INTEGER_TRUNCATE_INST_ID = 126
  val INTEGER_ZERO_EXTEND_INST_ID = 127

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
  val CHAR_TYPE_ID = 3
  val STRING_TYPE_ID = 4
  val INT_TYPE_ID = 5
  val FLOAT_TYPE_ID = 6
  val POINTER_TYPE_ID = 7
  val NULL_TYPE_ID = 8
  val ARRAY_TYPE_ID = 9
  val STRUCT_TYPE_ID = 10
  val FUNCTION_TYPE_ID = 11
  val CLASS_TYPE_ID = 12
  val INTERFACE_TYPE_ID = 13
  val VARIABLE_TYPE_ID = 14

  val UNIT_VALUE_ID = 1
  val BOOLEAN_VALUE_ID = 2
  val CHAR_VALUE_ID = 3
  val STRING_VALUE_ID = 4
  val INT8_VALUE_ID = 5
  val INT16_VALUE_ID = 6
  val INT32_VALUE_ID = 7
  val INT64_VALUE_ID = 8
  val FLOAT32_VALUE_ID = 9
  val FLOAT64_VALUE_ID = 10
  val NULL_VALUE_ID = 11
  val ARRAY_VALUE_ID = 12
  val STRUCT_VALUE_ID = 13
  val DEFINED_VALUE_ID = 14

  val COVARIANT_ID = 1
  val CONTRAVARIANT_ID = 2
  val INVARIANT_ID = 3
}
