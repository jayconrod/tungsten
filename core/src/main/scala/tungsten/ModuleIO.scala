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
      case Left(module) => {
        val errors = module.validate
        if (errors.isEmpty)
          Left(module)
        else
          Right(errors)
      }
      case errors => errors
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
      val empty = scala.collection.immutable.Map[Symbol, Definition]()
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
        case BLOCK_ID => Block(name, readList(symbol), readList(symbol), readAnnotations)
        case FIELD_ID => Field(name, readType, readAnnotations)
        case FUNCTION_ID => {
          Function(name, readList(symbol), readType, readList(symbol), readAnnotations)
        }
        case GLOBAL_ID => Global(name, readType, readOption(readValue), readAnnotations)
        case PARAMETER_ID => Parameter(name, readType, readAnnotations)
        case STRUCT_ID => Struct(name, readList(symbol), readAnnotations)

        case ADDRESS_INST_ID => {
          AddressInstruction(name, readValue, readList(readValue), readAnnotations)
        }
        case ASSIGN_INST_ID => AssignInstruction(name, readValue, readAnnotations)
        case BINARY_OPERATOR_INST_ID => {
          BinaryOperatorInstruction(name, readBinaryOperator, readValue, readValue, readAnnotations)
        }
        case BRANCH_INST_ID => {
          BranchInstruction(name, symbol, readList(readValue), readAnnotations)
        }
        case CONDITIONAL_BRANCH_INST_ID => {
          ConditionalBranchInstruction(name, readValue, symbol, readList(readValue),
                                       symbol, readList(readValue), readAnnotations)
        }
        case FLOAT_EXTEND_INST_ID => {
          FloatExtendInstruction(name, readValue, readType, readAnnotations)
        }
        case FLOAT_TO_INTEGER_INST_ID => {
          FloatToIntegerInstruction(name, readValue, readType, readAnnotations)
        }
        case FLOAT_TRUNCATE_INST_ID => {
          FloatTruncateInstruction(name, readValue, readType, readAnnotations)
        }
        case HEAP_ALLOCATE_INST_ID => {
          HeapAllocateInstruction(name, readType, readAnnotations)
        }
        case HEAP_ALLOCATE_ARRAY_INST_ID => {
          HeapAllocateArrayInstruction(name, readValue, readType, readAnnotations)
        }
        case INTEGER_SIGN_EXTEND_INST_ID => {
          IntegerSignExtendInstruction(name, readValue, readType, readAnnotations)
        }
        case INTEGER_TO_FLOAT_INST_ID => {
          IntegerToFloatInstruction(name, readValue, readType, readAnnotations)
        }
        case INTEGER_TRUNCATE_INST_ID => {
          IntegerTruncateInstruction(name, readValue, readType, readAnnotations)
        }
        case INTEGER_ZERO_EXTEND_INST_ID => {
          IntegerZeroExtendInstruction(name, readValue, readType, readAnnotations)
        }
        case INTRINSIC_CALL_INST_ID => {
          IntrinsicCallInstruction(name, readIntrinsic, readList(readValue), readAnnotations)
        }
        case LOAD_INST_ID => LoadInstruction(name, readValue, readAnnotations)
        case LOAD_ELEMENT_INST_ID => {
          LoadElementInstruction(name, readValue, readList(readValue), readAnnotations)
        }
        case RELATIONAL_OPERATOR_INST_ID => {
          RelationalOperatorInstruction(name, readRelationalOperator, 
                                        readValue, readValue, readAnnotations)
        }
        case RETURN_INST_ID => ReturnInstruction(name, readValue, readAnnotations)
        case STORE_INST_ID => StoreInstruction(name, readValue, readValue, readAnnotations)
        case STORE_ELEMENT_INST_ID => {
          StoreElementInstruction(name, readValue, readList(readValue), readValue, readAnnotations)
        }
        case STACK_ALLOCATE_INST_ID => StackAllocateInstruction(name, readType, readAnnotations)
        case STATIC_CALL_INST_ID => {
          StaticCallInstruction(name, symbol, readList(readValue), readAnnotations)
        }
        case UPCAST_INST_ID => UpcastInstruction(name, readValue, readType, readAnnotations)
        case _ => throw new IOException("Invalid definition ID")
      }
    }

    def readAnnotations: List[AnnotationValue] = {
      readList(readAnnotation)
    }

    def readAnnotation: AnnotationValue = {
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
          val size = readOption(readInt)
          size.filter(_ < 0).foreach { sz: Int => throw new IOException("Invalid array size: " + sz) }
          ArrayType(size, readType)
        }
        case STRUCT_TYPE_ID => StructType(symbol)
        case FUNCTION_TYPE_ID => FunctionType(readType, readList(readType))
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
        case DEFINED_VALUE_ID => DefinedValue(symbol)
        case _ => throw new IOException("Invalid value ID")
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
    Parser.phrase(Parser.module(file))(scanner) match {
      case Parser.Success((headers, asts), _) => {
        val definitions = asts.flatMap(processAst(_, Nil))
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
      case error: Parser.NoSuccess => Right(List(ParseException(error.msg, Nowhere)))
    }
  }            

  def processAst(ast: AstNode, scope: List[String]): List[Definition] = {
    val definition = ast.definition.mapSymbols(renameInScope(_, scope))
    val newScope = definition.name.name
    val children = ast.children.flatMap(processAst(_, newScope))
    definition :: children
  }    

  def renameInScope(symbol: Symbol, scope: List[String]): Symbol = {
    val name = symbol.name
    val prefix = name.head(0)
    val strippedName = name.head.substring(1) :: name.tail
    assert(prefix == '%' || prefix == '@')
    val isGlobal = prefix == '@'
    val newName = if (isGlobal)
      strippedName
    else
      scope ++ strippedName
    Symbol(newName, symbol.id)
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
          case g: Global => writeGlobal(g)
          case f: Function => writeFunction(f)
          case s: Struct => writeStruct(s)
          case _ => ()
        }
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
      writeFields(annotation.fields, Some(annotation.name))
    }

    def writeFunction(function: Function) {
      writeAnnotations(function.annotations)
      output.write("function ")
      writeType(function.returnType, None)
      output.write(" ")
      writeSymbol(function.name, None)
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
      writeInstructions(block.instructions, parentName)
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

    def writeInstruction(instruction: Instruction, parentName: Option[Symbol]) {
      val localName = this.localSymbol(instruction.name, parentName)
      def localSymbol(symbol: Symbol): String = this.localSymbol(symbol, parentName).toString
      def localValue(value: Value): String = this.localValue(value, parentName)
      def localType(ty: Type): String = this.localType(ty, parentName)

      output.write(INDENT + INDENT)
      writeAnnotations(instruction.annotations)
      instruction match {
        case AddressInstruction(_, base, indices, _) => {
          output.write("address " + localName + " = " + localValue(base) + ", " +
                       indices.map(localValue _).mkString(", "))
        }
        case AssignInstruction(_, value, _) => {
          output.write("assign " + localName + " = " + localValue(value))
        }
        case BinaryOperatorInstruction(_, operator, left, right, _) => {
          output.write("binop " + localName + " = " +
            localValue(left) + " " + operator.name + " " + localValue(right))
        }
        case BranchInstruction(_, target, arguments, _) => {
          output.write("branch " + localName + 
            " = " + localSymbol(target))
          writeArguments(arguments, parentName)
        }
        case ConditionalBranchInstruction(_, condition, trueTarget, trueArgs,
                                          falseTarget, falseArgs, _) =>
        {
          output.write("cond " + localName + " = " +
            localValue(condition) + " ? " + localSymbol(trueTarget))
          writeArguments(trueArgs, parentName)
          output.write(" : " + localSymbol(falseTarget))
          writeArguments(falseArgs, parentName)
        }
        case FloatExtendInstruction(_, value, ty, _) => {
          output.write("fextend " + localName + " = " + 
            localValue(value) + " to " + localType(ty))
        }
        case FloatToIntegerInstruction(_, value, ty, _) => {
          output.write("ftoi " + localName + " = " +
            localValue(value) + " to " + localType(ty))
        }
        case FloatTruncateInstruction(_, value, ty, _) => {
          output.write("ftruncate " + localName + " = " +
            localValue(value) + " to " + ty)
        }
        case HeapAllocateInstruction(_, ty, _) => {
          output.write("heap " + localName + " = " + localType(ty))
        }
        case HeapAllocateArrayInstruction(_, count, elementType, _) => {
          output.write("heaparray " + localName + " = " +
            localValue(count) + " x " + localType(elementType))
        }
        case IntegerSignExtendInstruction(_, value, ty, _) => {
          output.write("isextend " + localName + " = " +
            localValue(value) + " to " + localType(ty))
        }
        case IntegerToFloatInstruction(_, value, ty, _) => {
          output.write("itof " + localName + " = " +
            localValue(value) + " to " + localType(ty))
        }
        case IntegerTruncateInstruction(_, value, ty, _) => {
          output.write("itruncate " + localName + " = " +
            localValue(value) + " to " + localType(ty))
        }
        case IntegerZeroExtendInstruction(_, value, ty, _) => {
          output.write("izextend " + localName + " = " +
            localValue(value) + " to " + localType(ty))
        }
        case IntrinsicCallInstruction(_, intrinsic, arguments, _) => {
          output.write("intrinsic " + localName + 
            " = " + intrinsic.name)
          writeArguments(arguments, parentName)
        }
        case LoadInstruction(_, pointer, _) => {
          output.write("load " + localName + 
            " = " + localValue(pointer))
        }
        case LoadElementInstruction(_, base, indices, _) => {
          output.write("loadelement " + localName + " = " +
            base + ", " + indices.map(localValue _).mkString(", "))
        }
        case RelationalOperatorInstruction(_, operator, left, right, _) => {
          output.write("relop " + localName + " = " + 
            localValue(left) + " " + operator.name + " " + localValue(right))
        }
        case ReturnInstruction(_, value, _) => {
          output.write("return " + localName + 
            " = " + localValue(value))
        }
        case StackAllocateInstruction(_, ty, _) => {
          output.write("stack " + localName + " = " + localType(ty))
        }
        case StackAllocateArrayInstruction(_, count, elementType, _) => {
          output.write("stackarray " + localName + " = " +
            localValue(count) + " x " + localType(elementType))
        }
        case StaticCallInstruction(_, target, arguments, _) => {
          output.write("scall " + localName + " = " + localSymbol(target))
          writeArguments(arguments, parentName)
        }
        case StoreInstruction(_, pointer, value, _) => {
          output.write("store " + localName + " = " + 
            localValue(pointer) + ", " + localValue(value))
        }
        case StoreElementInstruction(_, base, indices, value, _) => {
          output.write("storeelement " + localName + " = " + 
            localValue(base) + ", " + indices.map(localValue _).mkString(", ") + ", " + 
            localValue(value))
        }
        case UpcastInstruction(_, value, ty, _) => {
          output.write("upcast " + localName + 
            " = " + localValue(value) + " to " + localType(ty))
        }
      }
      output.write("\n")
    }

    def writeAnnotations(annotations: List[AnnotationValue]) {
      writeChildren(annotations, writeAnnotationValue _, "", " ", " ")
    }

    def writeAnnotationValue(annotation: AnnotationValue) {
      writeSymbol(annotation.name, None)
      writeArguments(annotation.fields, None)
    }

    def writeArguments(values: List[Value], parentName: Option[Symbol]) {
      output.write(values.map(localValue(_, parentName)).mkString("(", ", ", ")"))
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

    def localValue(value: Value, parentName: Option[Symbol]): String = {
      value.mapSymbols(localSymbol(_, parentName)).toString
    }

    def localType(ty: Type, parentName: Option[Symbol]): String = {
      ty.mapSymbols(localSymbol(_, parentName)).toString
    }

    def localSymbol(symbol: Symbol, parentName: Option[Symbol]): Symbol = {
      def addPrefix(symbol: Symbol, prefix: Char): Symbol = {
        val fullName = symbol.name
        val newFullName = (prefix + fullName.head) :: fullName.tail
        Symbol(newFullName, symbol.id)
      }

      parentName match {
        case None => addPrefix(symbol, '@')
        case Some(parent) => {
          val simpleName = symbol.name.last
          if (parent.name :+ simpleName == symbol.name)
            addPrefix(Symbol(simpleName, symbol.id), '%')
          else
            addPrefix(symbol, '@')
        }
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

    def collect {
      module.definitions.values.foreach(collectDefinition _)
    }

    def collectDefinition(defn: Definition) {
      collectSymbol(defn.name)
      defn.annotations.flatMap(_.fields).foreach(collectValue _)
      defn match {
        case _: Annotation => ()
        case _: Block => ()
        case Field(_, ty, _) => collectType(ty)
        case Function(_, _, returnType, _, _) => collectType(returnType)
        case Global(_, ty, value, _) => {
          collectType(ty)
          value.foreach(collectValue _)
        }
        case Parameter(_, ty, _) => collectType(ty)
        case _: Struct => ()

        case inst: Instruction => {
          inst.operands.foreach(collectValue _)
          inst match {
            case _: AddressInstruction => ()
            case _: AssignInstruction => ()
            case _: BinaryOperatorInstruction => ()
            case _: BranchInstruction => ()
            case _: ConditionalBranchInstruction => ()
            case FloatExtendInstruction(_, _, ty, _) => collectType(ty)
            case FloatToIntegerInstruction(_, _, ty, _) => collectType(ty)
            case FloatTruncateInstruction(_, _, ty, _) => collectType(ty)
            case HeapAllocateInstruction(_, ty, _) => collectType(ty)
            case HeapAllocateArrayInstruction(_, _, ty, _) => collectType(ty)
            case IntegerSignExtendInstruction(_, _, ty, _) => collectType(ty)
            case IntegerToFloatInstruction(_, _, ty, _) => collectType(ty)
            case IntegerTruncateInstruction(_, _, ty, _) => collectType(ty)
            case IntegerZeroExtendInstruction(_, _, ty, _) => collectType(ty)
            case _: IntrinsicCallInstruction => ()
            case _: LoadInstruction => ()
            case _: LoadElementInstruction => ()
            case _: RelationalOperatorInstruction => ()
            case _: ReturnInstruction => ()
            case _: StoreInstruction => ()
            case _: StoreElementInstruction => ()
            case StackAllocateInstruction(_, ty, _) => collectType(ty)
            case StackAllocateArrayInstruction(_, _, ty, _) => collectType(ty)
            case _: StaticCallInstruction => ()
            case UpcastInstruction(_, _, ty, _) => collectType(ty)
          }
        }
      }
    }

    def collectType(ty: Type) {
      ty match {
        case UnitType => ()
        case BooleanType => ()
        case CharType => ()
        case StringType => ()
        case _: IntType => ()
        case _: FloatType => ()
        case PointerType(elementType) => collectType(elementType)
        case NullType => ()
        case ArrayType(_, elementType) => collectType(elementType)
        case StructType(name) => collectSymbol(name)
        case FunctionType(returnType, parameterTypes) => {
          collectType(returnType)
          parameterTypes.foreach(collectType _)
        }
      }
    }

    def collectValue(value: Value) {
      value match {
        case UnitValue => ()
        case _: BooleanValue => ()
        case _: CharValue => ()
        case StringValue(s) => collectString(s)
        case _: IntValue => ()
        case _: FloatValue => ()
        case NullValue => ()
        case ArrayValue(elementType, elements) => {
          collectType(elementType)
          elements.foreach(collectValue _)
        }
        case StructValue(name, fields) => {
          collectSymbol(name)
          fields.foreach(collectValue _)
        }
        case _: DefinedValue => ()
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
        case Block(_, parameters, instructions, _) => {
          output.writeByte(BLOCK_ID)
          writeSymbolList(parameters)
          writeSymbolList(instructions)
        }
        case Field(_, ty, _) => {
          output.writeByte(FIELD_ID)
          writeType(ty)
        }
        case Function(_, parameters, returnType, blocks, _) => {
          output.writeByte(FUNCTION_ID)
          writeSymbolList(parameters)
          writeType(returnType)
          writeSymbolList(blocks)
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
          writeSymbolList(fields)
        }

        case inst: Instruction => inst match {
          case AddressInstruction(_, base, indices, _) => {
            output.writeByte(ADDRESS_INST_ID)
            writeValue(base)
            writeList(indices, writeValue _)
          }
          case AssignInstruction(_, value, _) => {
            output.writeByte(ASSIGN_INST_ID)
            writeValue(value)
          }
          case BinaryOperatorInstruction(_, operator, left, right, _) => {
            output.writeByte(BINARY_OPERATOR_INST_ID)
            writeBinaryOperator(operator)
            writeValue(left)
            writeValue(right)
          }
          case BranchInstruction(_, target, arguments, _) => {
            output.writeByte(BRANCH_INST_ID)
            writeInt(symbols(target))
            writeList(arguments, writeValue _)
          }
          case ConditionalBranchInstruction(_, condition, trueTarget, trueArgs,
                                            falseTarget, falseArgs, _) =>
          {
            output.writeByte(CONDITIONAL_BRANCH_INST_ID)
            writeValue(condition)
            writeInt(symbols(trueTarget))
            writeList(trueArgs, writeValue _)
            writeInt(symbols(falseTarget))
            writeList(falseArgs, writeValue _)
          }
          case FloatExtendInstruction(_, value, ty, _) => {
            output.writeByte(FLOAT_EXTEND_INST_ID)
            writeValue(value)
            writeType(ty)
          }
          case FloatToIntegerInstruction(_, value, ty, _) => {
            output.writeByte(FLOAT_TO_INTEGER_INST_ID)
            writeValue(value)
            writeType(ty)
          }
          case FloatTruncateInstruction(_, value, ty, _) => {
            output.writeByte(FLOAT_TRUNCATE_INST_ID)
            writeValue(value)
            writeType(ty)
          }
          case HeapAllocateInstruction(_, ty, _) => {
            output.writeByte(HEAP_ALLOCATE_INST_ID)
            writeType(ty)
          }
          case HeapAllocateArrayInstruction(_, count, elementType, _) => {
            output.writeByte(HEAP_ALLOCATE_ARRAY_INST_ID)
            writeValue(count)
            writeType(elementType)
          }
          case IntegerSignExtendInstruction(_, value, ty, _) => {
            output.writeByte(INTEGER_SIGN_EXTEND_INST_ID)
            writeValue(value)
            writeType(ty)
          }
          case IntegerToFloatInstruction(_, value, ty, _) => {
            output.writeByte(INTEGER_TO_FLOAT_INST_ID)
            writeValue(value)
            writeType(ty)
          }
          case IntegerTruncateInstruction(_, value, ty, _) => {
            output.writeByte(INTEGER_TRUNCATE_INST_ID)
            writeValue(value)
            writeType(ty)
          }
          case IntegerZeroExtendInstruction(_, value, ty, _) => {
            output.writeByte(INTEGER_ZERO_EXTEND_INST_ID)
            writeValue(value)
            writeType(ty)
          }
          case IntrinsicCallInstruction(_, intrinsic, arguments, _) => {
            output.writeByte(INTRINSIC_CALL_INST_ID)
            writeIntrinsic(intrinsic)
            writeList(arguments, writeValue _)
          }
          case LoadInstruction(_, pointer, _) => {
            output.writeByte(LOAD_INST_ID)
            writeValue(pointer)
          }
          case LoadElementInstruction(_, base, indices, _) => {
            output.writeByte(LOAD_ELEMENT_INST_ID)
            writeValue(base)
            writeList(indices, writeValue _)
          }
          case RelationalOperatorInstruction(_, operator, left, right, _) => {
            output.writeByte(RELATIONAL_OPERATOR_INST_ID)
            writeRelationalOperator(operator)
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
            writeList(indices, writeValue _)
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
            writeInt(symbols(target))
            writeList(arguments, writeValue _)
          }
          case UpcastInstruction(_, value, ty, _) => {
            output.writeByte(UPCAST_INST_ID)
            writeValue(value)
            writeType(ty)
          }
        }
      }
      writeList(defn.annotations, writeAnnotationValue _)
    }

    def writeAnnotationValue(av: AnnotationValue) {
      writeInt(symbols(av.name))
      writeList(av.fields, writeValue _)
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
        case ArrayType(size, elementType) => {
          output.writeByte(ARRAY_TYPE_ID)
          writeOption(size, writeInt _)
          writeType(elementType)
        }
        case StructType(structName) => {
          output.writeByte(STRUCT_TYPE_ID)
          writeInt(symbols(structName))
        }
        case FunctionType(returnType, parameterTypes) => {
          output.writeByte(FUNCTION_TYPE_ID)
          writeType(returnType)
          writeList(parameterTypes, writeType _)
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
        case DefinedValue(name) => {
          output.writeByte(DEFINED_VALUE_ID)
          writeInt(symbols(name))
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
  // 105 available
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
  val HEAP_ALLOCATE_INST_ID = 117
  val HEAP_ALLOCATE_ARRAY_INST_ID = 118
  val FLOAT_EXTEND_INST_ID = 119
  val FLOAT_TO_INTEGER_INST_ID = 120
  val FLOAT_TRUNCATE_INST_ID = 121
  val INTEGER_SIGN_EXTEND_INST_ID = 122
  val INTEGER_TO_FLOAT_INST_ID = 123
  val INTEGER_TRUNCATE_INST_ID = 124
  val INTEGER_ZERO_EXTEND_INST_ID = 125

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
}
