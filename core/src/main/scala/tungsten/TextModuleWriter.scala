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

import java.io._
import Utilities._

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
    writeParameters(annotation.parameters, Some(annotation.name))
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

    if (!clas.interfaceTypes.isEmpty || 
        !clas.constructors.isEmpty || 
        !clas.methods.isEmpty || 
        !clas.fields.isEmpty)
    {
      output.write(" {\n")

      writeImplementedInterfaces(clas.interfaceTypes, clas.interfaceMethods, Some(clas.name))

      if (!clas.constructors.isEmpty) {
        output.write(INDENT + "constructors")
        writeSymbolBlock(clas.constructors, Some(clas.name))
        output.write("\n")
      }

      if (!clas.methods.isEmpty) {
        output.write(INDENT + "methods")
        writeSymbolBlock(clas.methods, Some(clas.name))
        output.write("\n")
      }

      if (!clas.fields.isEmpty) {
        val fields = module.getFields(clas.fields)
        writeChildren(fields, writeField(_: Field, Some(clas.name)), "", "\n", "\n")
      }

      output.write("}")
    }
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

    if (!interface.interfaceTypes.isEmpty ||
        !interface.methods.isEmpty)
    {
      output.write(" {\n")

      writeImplementedInterfaces(interface.interfaceTypes, 
                                 interface.interfaceMethods, 
                                 Some(interface.name))

      if (!interface.methods.isEmpty) {
        output.write(INDENT + "methods")
        writeSymbolBlock(interface.methods, Some(interface.name))
        output.write("\n}")      
      }
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
      case _: NewInstruction => "new"
      case _: PointerCallInstruction => "pcall"
      case _: RelationalOperatorInstruction => "relop"
      case _: ReturnInstruction => "return"
      case _: StoreInstruction => "store"
      case _: StoreElementInstruction => "storeelement"
      case _: StackAllocateInstruction => "stack"
      case _: StackAllocateArrayInstruction => "stackarray"
      case _: StaticCallInstruction => "scall"
      case _: UpcastInstruction => "upcast"
      case _: VirtualCallInstruction => "vcall"
    }

    output.write(INDENT + INDENT)
    writeAnnotations(instruction.annotations)
    output.write(localType(instruction.ty) + " " + localName + " = " + instName + " ")
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
      case NewInstruction(_, _, constructor, typeArguments, arguments, _) => {
        output.write(localSymbol(constructor))
        writeTypeArguments(typeArguments, parentName)
        writeArguments(arguments, parentName)
      }
      case PointerCallInstruction(_, _, target, typeArguments, arguments, _) => {
        output.write(localValue(target))
        writeTypeArguments(typeArguments, parentName)
        writeArguments(arguments, parentName)
      }
      case LoadInstruction(_, _, pointer, _) => {
        output.write(localValue(pointer))
      }
      case LoadElementInstruction(_, _, base, indices, _) => {
        output.write(localValue(base) + ", " + indices.map(localValue _).mkString(", "))
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
      case StaticCallInstruction(_, _, target, typeArguments, arguments, _) => {
        output.write(localSymbol(target))
        writeTypeArguments(typeArguments, parentName)
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
      case VirtualCallInstruction(_, _, target, methodIndex, typeArguments, arguments, _) => {
        output.write(localValue(target) + ":" + methodIndex)
        writeTypeArguments(typeArguments, parentName)
        writeArguments(arguments, parentName)
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

  def writeTypeArguments(typeArguments: List[Type], parentName: Option[Symbol]) {
    if (!typeArguments.isEmpty)
      output.write(typeArguments.map(localType(_, parentName)).mkString("[", ", ", "]"))
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
      case BitCastValue(value, ty) => {
        "bitcast %s to %s".format(localValue(value, parentName), localType(ty, parentName))
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
    def localTypeParameters(typeParameters: List[Symbol]): String = {
      if (typeParameters.isEmpty)
        ""
      else
        typeParameters.map(localSymbol(_, parentName)).mkString("[", ", ", "]")
    }
    def localParameterTypes(parameterTypes: List[Type]): String = {
      if (parameterTypes.isEmpty)
        ""
      else
        parameterTypes.map(localType(_, parentName)).mkString("(", ", ", ")")
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
        val typeParametersStr = localTypeParameters(typeParameters)
        val parameterTypesStr = localParameterTypes(parameterTypes)
        val returnTypeStr = localType(returnType, parentName)
        "%s%s->%s".format(typeParametersStr, parameterTypesStr, returnTypeStr)
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
