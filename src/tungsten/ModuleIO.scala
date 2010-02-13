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
    val locations = new ArrayBuffer[Location]
    val symbols = new ArrayBuffer[Symbol]

    def read: Module = {
      val header = readHeader
      readTable(strings, readString)
      readTable(locations, readLocation)
      readTable(symbols, readSymbol)
      val empty = scala.collection.immutable.Map[Symbol, Definition]()
      val definitions = (0 until symbols.size).foldLeft(empty) { (definitions, _) =>
        val defn = readDefinition
        if (definitions.contains(defn.name))
          throw new IOException("Duplicate definition")
        definitions + (defn.name -> defn)
      }
      new Module(header._1, header._2, header._3, header._4, header._5, header._6, definitions)
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
      (name, ty, version, filename, dependencies, searchPaths)
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
        case BLOCK_ID => Block(name, readList(symbol), readList(symbol), location)
        case FIELD_ID => Field(name, readType, location)
        case FUNCTION_ID => {
          Function(name, readList(symbol), readType, readList(symbol), location)
        }
        case GLOBAL_ID => Global(name, readType, readOption(readValue), location)
        case PARAMETER_ID => Parameter(name, readType, location)
        case STRUCT_ID => Struct(name, readList(symbol), location)

        case ADDRESS_INST_ID => {
          AddressInstruction(name, readValue, readList(readValue), location)
        }
        case ASSIGN_INST_ID => AssignInstruction(name, readValue, location)
        case BINARY_OPERATOR_INST_ID => {
          BinaryOperatorInstruction(name, readBinaryOperator, readValue, readValue, location)
        }
        case BRANCH_INST_ID => {
          BranchInstruction(name, symbol, readList(readValue), location)
        }
        case CONDITIONAL_BRANCH_INST_ID => {
          ConditionalBranchInstruction(name, readValue, symbol, readList(readValue),
                                       symbol, readList(readValue), location)
        }
        case HEAP_ALLOCATE_INST_ID => {
          HeapAllocateInstruction(name, readType, location)
        }
        case HEAP_ALLOCATE_ARRAY_INST_ID => {
          HeapAllocateArrayInstruction(name, readValue, readType, location)
        }
        case INDIRECT_CALL_INST_ID => {
          IndirectCallInstruction(name, readValue, readList(readValue), location)
        }
        case INTRINSIC_CALL_INST_ID => {
          IntrinsicCallInstruction(name, readIntrinsic, readList(readValue), location)
        }
        case LOAD_INST_ID => LoadInstruction(name, readValue, location)
        case LOAD_ELEMENT_INST_ID => {
          LoadElementInstruction(name, readValue, readList(readValue), location)
        }
        case RELATIONAL_OPERATOR_INST_ID => {
          RelationalOperatorInstruction(name, readRelationalOperator, 
                                        readValue, readValue, location)
        }
        case RETURN_INST_ID => ReturnInstruction(name, readValue, location)
        case STORE_INST_ID => StoreInstruction(name, readValue, readValue, location)
        case STORE_ELEMENT_INST_ID => {
          StoreElementInstruction(name, readValue, readList(readValue), readValue, location)
        }
        case STACK_ALLOCATE_INST_ID => StackAllocateInstruction(name, readType, location)
        case STATIC_CALL_INST_ID => {
          StaticCallInstruction(name, symbol, readList(readValue), location)
        }
        case UPCAST_INST_ID => UpcastInstruction(name, readValue, readType, location)
        case _ => throw new IOException("Invalid definition ID")
      }
    }

    def readType: Type = {
      input.readByte match {
        case UNIT_TYPE_ID => UnitType(location)
        case BOOLEAN_TYPE_ID => BooleanType(location)
        case INT_TYPE_ID => {
          val width = input.readByte.asInstanceOf[Int]
          if (!List(8, 16, 32, 64).contains(width))
            throw new IOException("Invalid integer type")
          IntType(width, location)
        }
        case FLOAT_TYPE_ID => {
          val width = input.readByte
          if (!List(32, 64).contains(width))
            throw new IOException("Invalid float type")
          FloatType(width, location)
        }
        case POINTER_TYPE_ID => PointerType(readType, location)
        case NULL_TYPE_ID => NullType(location)
        case ARRAY_TYPE_ID => {
          val size = readOption(readInt)
          size.filter(_ < 0).foreach(throw new IOException("Invalid array type"))
          ArrayType(size, readType, location)
        }
        case STRUCT_TYPE_ID => StructType(symbol, location)
        case FUNCTION_TYPE_ID => FunctionType(readType, readList(readType), location)
        case _ => throw new IOException("Invalid type ID")
      }
    }

    def readValue: Value = {
      input.readByte match {
        case UNIT_VALUE_ID => UnitValue(location)
        case BOOLEAN_VALUE_ID => {
          val value = input.readByte match {
            case 0 => false
            case 1 => true
            case _ => throw new IOException("Invalid Boolean value")
          }
          BooleanValue(value, location)
        }
        case INT8_VALUE_ID => Int8Value(input.readByte, location)
        case INT16_VALUE_ID => Int16Value(input.readShort, location)
        case INT32_VALUE_ID => Int32Value(input.readInt, location)
        case INT64_VALUE_ID => Int64Value(input.readLong, location)
        case FLOAT32_VALUE_ID => Float32Value(input.readFloat, location)
        case FLOAT64_VALUE_ID => Float64Value(input.readDouble, location)
        case NULL_VALUE_ID => NullValue(location)
        case ARRAY_VALUE_ID => ArrayValue(readType, readList(readValue), location)
        case STRUCT_VALUE_ID => StructValue(symbol, readList(readValue), location)
        case DEFINED_VALUE_ID => DefinedValue(symbol, location)
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

    def readLocation: Location = {
      Location(get(strings, readInt), readInt, readInt, readInt, readInt)
    }

    def location = get(locations, readInt)

    def symbol = get(symbols, readInt)

    def get[T](table: ArrayBuffer[T], index: Int) = {
      if (!table.indices.contains(index))
        throw new IOException("Invalid table index")
      table(index)
    }

    def readString: String = input.readUTF

    def readInt: Int = input.readInt

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
  def parse(text: String, filename: String): Either[AstModule, String] = {
    val file = Some(new File(filename))
    val reader = new CharSequenceReader(text)
    val scanner = new AstLexer.Scanner(filename, reader)
    AstParser.phrase(AstParser.module(file))(scanner) match {
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

    def write {
      writeHeader
      for (defn <- module.definitions.valuesIterable
           if isTopLevel(defn))
        writeDefinition(defn)
    }

    def writeHeader {
      output.write("#name " + module.name + "\n")
      val tyStr = module.ty match {
        case ModuleType.INTERMEDIATE => "#intermediate"
        case ModuleType.LIBRARY => "#library"
        case ModuleType.PROGRAM => "#program"
      }
      output.write("#type " + tyStr + "\n")
      if (module.version != Version.MIN)
        output.write("#version " + module.version + "\n")
      module.filename match {
        case Some(filename) => output.write("#filename \"" + filename + "\"\n")
        case _ => ()
      }
      if (!module.dependencies.isEmpty)
        output.write("#dependencies " + module.dependencies.mkString(", ") + "\n")
      if (!module.searchPaths.isEmpty)
        output.write("#searchpaths " + module.searchPaths.mkString(", ") + "\n")
      output.write("\n")
    }

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
      output.write(INDENT + "#block " + locationString(block.location) + localSymbol(block.name))
      parentNames.push(block.name)
      writeParameterList(block.parameters)
      if (block.instructions.isEmpty)
        output.write("\n")
      else {
        output.write(" {\n")
        parentNames.push(block.name)
        writeDefinitionList(block.instructions, "")
        parentNames.pop
        output.write(INDENT + "}\n")
      }
      parentNames.pop
    }

    def writeDefinition(field: Field) {
      output.write(INDENT + "#field " + locationString(field.location) + 
        localSymbol(field.name) + ": " + field.ty + "\n")
    }

    def writeDefinition(function: Function) {
      output.write("#function " + locationString(function.location) + function.name)
      parentNames.push(function.name)
      writeParameterList(function.parameters)
      output.write(": " + function.returnType)
      if (function.blocks.isEmpty)
        output.write("\n")
      else {
        output.write(" {\n")
        writeDefinitionList(function.blocks, "")
        output.write("}\n")
      }
      parentNames.pop
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
      output.write(localSymbol(param.name).toString + locStr + ": " + param.ty)
    }

    def writeDefinition(struct: Struct) {
      output.write("#struct " + locationString(struct.location) + struct.name)
      parentNames.push(struct.name)
      if (struct.fields.isEmpty)
        output.write("\n")
      else {
        output.write(" {\n")
        writeDefinitionList(struct.fields, "")
        output.write("}\n")
      }
      parentNames.pop
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
        case HeapAllocateInstruction(name, ty, location) => {
          output.write("#heap " + locationString(location) + localName + ": " + ty)
        }
        case HeapAllocateArrayInstruction(name, count, elementType, location) => {
          output.write("#heaparray " + locationString(location) + localName + " = " +
            count + " * " + elementType)
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

    /** Returns the simple version of the symbol if the simple name by itself does not refer 
     *  to another definition, nor does any prefix combined with the simple name. The symbol
     *  on top of the parent name stack combined with the simple name must also match the
     *  whole name. Otherwise, the given name is returned.
     */
    def localSymbol(sym: Symbol): Symbol = {
      def conflicts(name: Symbol): Boolean = {
        module.getDefn(name) match {
          case Some(defn) if defn.name != sym => true
          case _ => false
        }
      }

      val matchesPrefix = (false /: parentNames) { (b, prefix) =>
        b || prefix + sym.simple == sym
      }
      if (conflicts(sym.simple) ||
          parentNames.exists { (prefix: Symbol) => conflicts(prefix + sym.simple) } ||
          !matchesPrefix)
        sym
      else
        sym.simple
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
    val locations = new Table[Location] 
    val symbols = new Table[Symbol]

    def write {
      collect
      writeHeader
      strings.write(writeString _)
      locations.write(writeLocation _)
      symbols.write(writeSymbol _)
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
            case _: IndirectCallInstruction => ()
            case _: IntrinsicCallInstruction => ()
            case HeapAllocateInstruction(_, ty, _) => collectType(ty)
            case HeapAllocateArrayInstruction(_, _, ty, _) => collectType(ty)
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
      collectLocation(ty.location)
      ty match {
        case _: UnitType => ()
        case _: BooleanType => ()
        case _: IntType => ()
        case _: FloatType => ()
        case PointerType(elementType, _) => collectType(elementType)
        case _: NullType => ()
        case ArrayType(_, elementType, _) => collectType(elementType)
        case _: StructType => ()
        case FunctionType(returnType, parameterTypes, _) => {
          collectType(returnType)
          parameterTypes.foreach(collectType _)
        }
      }
    }

    def collectValue(value: Value) {
      collectLocation(value.location)
      value match {
        case _: UnitValue => ()
        case _: BooleanValue => ()
        case _: Int8Value | _: Int16Value | _: Int32Value | _: Int64Value => ()
        case _: Float32Value | _: Float64Value => ()
        case _: NullValue => ()
        case ArrayValue(elementType, elements, _) => {
          collectType(elementType)
          elements.foreach(collectValue _)
        }
        case StructValue(_, fields, _) => fields.foreach(collectValue _)
        case _: DefinedValue => ()
      }
    }

    def collectSymbol(symbol: Symbol) {
      symbol.name.foreach(strings.add(_))
      symbols.add(symbol)
    }

    def collectLocation(loc: Location) {
      strings.add(loc.filename)
      locations.add(loc)
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
    }

    def writeHeaderSymbol(sym: Symbol) {
      writeList(sym.name.toList, output.writeUTF _)
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
          case HeapAllocateInstruction(_, ty, _) => {
            output.writeByte(HEAP_ALLOCATE_INST_ID)
            writeType(ty)
          }
          case HeapAllocateArrayInstruction(_, count, elementType, _) => {
            output.writeByte(HEAP_ALLOCATE_ARRAY_INST_ID)
            writeValue(count)
            writeType(elementType)
          }
          case IndirectCallInstruction(_, target, arguments, _) => {
            output.writeByte(INDIRECT_CALL_INST_ID)
            writeValue(target)
            writeList(arguments, writeValue _)
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
      writeInt(locations(defn.location))
    }

    def writeType(ty: Type) {
      ty match {
        case UnitType(_) => output.writeByte(UNIT_TYPE_ID)
        case BooleanType(_) => output.writeByte(BOOLEAN_TYPE_ID)
        case IntType(width, _) => {
          output.writeByte(INT_TYPE_ID)
          output.writeByte(width)
        }
        case FloatType(width, _) => {
          output.writeByte(FLOAT_TYPE_ID)
          output.writeByte(width)
        }
        case PointerType(elementType, _) => {
          output.writeByte(POINTER_TYPE_ID)
          writeType(elementType)
        }
        case NullType(_) => output.writeByte(NULL_TYPE_ID)
        case ArrayType(size, elementType, _) => {
          output.writeByte(ARRAY_TYPE_ID)
          writeOption(size, writeInt _)
          writeType(elementType)
        }
        case StructType(structName, _) => {
          output.writeByte(STRUCT_TYPE_ID)
          writeInt(symbols(structName))
        }
        case FunctionType(returnType, parameterTypes, _) => {
          output.writeByte(FUNCTION_TYPE_ID)
          writeType(returnType)
          writeList(parameterTypes, writeType _)
        }
      }
      writeInt(locations(ty.location))
    }

    def writeValue(value: Value) {
      value match {
        case UnitValue(_) => output.writeByte(UNIT_VALUE_ID)
        case BooleanValue(b, _) => {
          output.writeByte(BOOLEAN_VALUE_ID)
          output.writeByte(if (b) 1 else 0)
        }
        case Int8Value(b, _) => {
          output.writeByte(INT8_VALUE_ID)
          output.writeByte(b)
        }
        case Int16Value(s, _) => {
          output.writeByte(INT16_VALUE_ID)
          output.writeShort(s)
        }
        case Int32Value(i, _) => {
          output.writeByte(INT32_VALUE_ID)
          output.writeInt(i)
        }
        case Int64Value(l, _) => {
          output.writeByte(INT64_VALUE_ID)
          output.writeLong(l)
        }
        case Float32Value(f, _) => {
          output.writeByte(FLOAT32_VALUE_ID)
          output.writeFloat(f)
        }
        case Float64Value(d, _) => {
          output.writeByte(FLOAT64_VALUE_ID)
          output.writeDouble(d)
        }
        case NullValue(_) => output.writeByte(NULL_VALUE_ID)
        case ArrayValue(elementType, elements, _) => {
          output.writeByte(ARRAY_VALUE_ID)
          writeType(elementType)
          writeList(elements, writeValue _)
        }
        case StructValue(structName, fields, _) => {
          output.writeByte(STRUCT_VALUE_ID)
          writeInt(symbols(structName))
          writeList(fields, writeValue _)
        }
        case DefinedValue(name, _) => {
          output.writeByte(DEFINED_VALUE_ID)
          writeInt(symbols(name))
        }
      }
      writeInt(locations(value.location))
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
      writeList(symbol.name.toList, { (n: String) => writeInt(strings(n)) })
      writeInt(symbol.id)
    }

    def writeLocation(loc: Location) {
      writeInt(strings(loc.filename))
      writeInt(loc.beginLine)
      writeInt(loc.beginColumn)
      writeInt(loc.endLine)
      writeInt(loc.endColumn)
    }

    def writeString(str: String) {
      output.writeUTF(str)
    }

    def writeInt(n: Int) {
      output.writeInt(n)
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
  val HEAP_ALLOCATE_INST_ID = 117
  val HEAP_ALLOCATE_ARRAY_INST_ID = 118

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
  val FUNCTION_TYPE_ID = 9

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
}
