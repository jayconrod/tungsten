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

import Utilities._

class LowerPass
  extends Function1[Module, Module]
{
  private var symbolFactory: SymbolFactory = new SymbolFactory

  def apply(module: Module) = {
    symbolFactory = new SymbolFactory(module.highestSymbolId + 1)
    val interfaceBaseClassNames = findInterfaceBaseClassNames(module)
    var m = module
    m = addDefinitions(m)
    m = convertClassesAndInterfaces(m)
    m = convertInstructions(m)
    m = convertFunctions(m)
    m = removeDefinitions(m)
    m = substituteTypes(interfaceBaseClassNames, m)
    m
  }

  def findInterfaceBaseClassNames(module: Module): Map[Symbol, Symbol] = {
    val interfaces = module.definitions.values.collect { case i: Interface => i }
    interfaces.map { i => (i.name, i.baseClass(module).name) }.toMap
  }

  def addDefinitions(module: Module): Module = {
    val resourceName = if (module.is64Bit) "lower-defns-64.w" else "lower-defns-32.w"
    val input = getClass.getResourceAsStream(resourceName)
    val reader = new java.io.InputStreamReader(input)
    val defns = ModuleIO.readText(reader, resourceName)
    Linker.linkModules(List(module, defns))
  }

  def convertClassesAndInterfaces(module: Module): Module = {
    val classes = module.definitions.values collect { case c: Class => c }
    val interfaces = module.definitions.values collect { case i: Interface => i }
    val ivtableMaps = classes.map { c => (c.name, c.getIVTables(module)) }.toMap

    var m = module
    for (i <- interfaces)
      m = convertInterface(i, m)
    for (c <- classes)
      m = convertClass(c, ivtableMaps(c.name), m)
    m
  }

  def convertInterface(interface: Interface, module: Module): Module = {
    var m = module
    m = createInterfaceInfo(interface, m)
    m = createVTableStruct(interface, m)
    m
  }

  def convertClass(clas: Class, 
                   ivtableMap: Map[Symbol, Either[List[Symbol], Symbol]],
                   module: Module): Module = 
  {
    var m = module
    m = createClassInfo(clas, m)
    m = createIVTableGlobals(clas, ivtableMap, m)
    m = createITableGlobal(clas, ivtableMap, m)
    m = createVTableStruct(clas, m)
    m = createVTableGlobal(clas, ivtableMap.size, m)
    m = createClassStruct(clas, m)
    m
  }

  def createClassInfo(clas: Class, module: Module): Module = {
    val nameValue = ArrayValue.fromString(clas.name.toString)
    val nameGlobal = Global(classNameName(clas.name),
                            nameValue.ty,
                            Some(nameValue))
    val infoValue = StructValue(classInfoStructName,
                                List(StructValue(arrayStructName,
                                                 List(BitCastValue(nameGlobal.makeValue,
                                                                   PointerType(IntType(8))),
                                                      IntValue.word(nameValue.elements.size, 
                                                                    module)))))
    val infoGlobal = Global(classInfoGlobalName(clas.name),
                            infoValue.ty,
                            Some(infoValue))
    module.add(nameGlobal, infoGlobal)
  }

  def createInterfaceInfo(interface: Interface, module: Module): Module = {
    val nameValue = ArrayValue.fromString(interface.name.toString)
    val nameGlobal = Global(interfaceNameName(interface.name),
                            nameValue.ty,
                            Some(nameValue))
    val infoValue = StructValue(interfaceInfoStructName,
                                List(StructValue(arrayStructName,
                                                 List(BitCastValue(nameGlobal.makeValue,
                                                                   PointerType(IntType(8))),
                                                      IntValue.word(nameValue.elements.size,
                                                                    module)))))
    val infoGlobal = Global(interfaceInfoGlobalName(interface.name),
                            infoValue.ty,
                            Some(infoValue))
    module.add(nameGlobal, infoGlobal)
  }

  def createClassStruct(clas: Class, module: Module): Module = {
    val vtableField = Field(vtablePtrName(clas.name),
                            PointerType(StructType(vtableStructName(clas.name))))
    val structFieldNames = vtableField.name :: clas.fields
    val struct = Struct(classStructName(clas.name), structFieldNames)
    module.add(vtableField, struct)
  }

  def createVTableStruct(defn: ObjectDefinition, module: Module): Module = {
    val vtableName = vtableStructName(defn.name)
    val methods = module.getFunctions(defn.methods)
    val methodFields = ((0 until methods.size).toList zip methods) map { p =>
      val (methodIndex, method) = p
      val fieldName = vtableFieldName(vtableName, method.name, methodIndex)
      Field(fieldName, method.ty(module))
    }
    val vtableFields = if (defn.isInstanceOf[Class]) {
      val infoType = PointerType(StructType(classInfoStructName))
      val infoField = Field(vtableClassInfoName(vtableName), infoType)

      val itableType = StructType(arrayStructName)
      val itableField = Field(vtableITableArrayName(vtableName), itableType)

      infoField :: itableField :: methodFields
    } else
      methodFields

    val vtableStruct = Struct(vtableName, vtableFields.map(_.name))
    module.add(vtableFields: _*).add(vtableStruct)
  }

  def createVTableGlobal(clas: Class, itableSize: Int, module: Module): Module = {
    val itableValues = createVTableHeaderValues(clas.name, itableSize, module)
    val methods = module.getFunctions(clas.methods)
    val methodValues = methods.map { m => DefinedValue(m.name, m.ty(module)) }
    val vtableValue = StructValue(vtableStructName(clas.name), 
                                  itableValues ++ methodValues)
    val vtableGlobal = Global(vtableGlobalName(clas.name),
                              StructType(vtableStructName(clas.name)),
                              Some(vtableValue))
    module.add(vtableGlobal)
  }

  def createIVTableGlobals(clas: Class,
                           ivtableMap: Map[Symbol, Either[List[Symbol], Symbol]],
                           module: Module): Module =
  {
    val realIVTables = ivtableMap.filter { kv => kv._2.isLeft }.mapValues(_.left.get)
    val ivtableGlobals = realIVTables.toList.map { p =>
      val (interfaceName, methodNames) = p
      val interface = module.getInterface(interfaceName)
      val methodTypes = module.getFunctions(interface.methods).map(_.ty(module))
      val methods = module.getFunctions(methodNames)
      val ivtableMethodValues = (methods zip methodTypes) map { p =>
        val (method, methodType) = p
        val methodValue = DefinedValue(method.name, method.ty(module))
        BitCastValue(methodValue, methodType)
      }
      val ivtableValue = StructValue(vtableStructName(interfaceName), ivtableMethodValues)
      Global(ivtableGlobalName(clas.name, interfaceName),
                               StructType(vtableStructName(interfaceName)),
                               Some(ivtableValue))
    }
    module.add(ivtableGlobals: _*)
  }

  def createVTableHeaderValues(className: Symbol, 
                               itableSize: Int, 
                               module: Module): List[Value] = 
  {
    val classInfoName = classInfoGlobalName(className)
    val classInfoValue = DefinedValue(classInfoName, 
                                      PointerType(StructType(classInfoStructName)))

    val itableGlobalType = PointerType(ArrayType(itableSize, StructType(itableEntryStructName)))
    val itableType = PointerType(IntType(8))
    val itableGlobalValue = DefinedValue(itableGlobalName(className), itableGlobalType)
    val itableValue = BitCastValue(itableGlobalValue, itableType)
    val itableSizeValue = IntValue(itableSize, IntType.wordSize(module))
    val itableArrayValue = StructValue(arrayStructName, List(itableValue, itableSizeValue))

    List(classInfoValue, itableArrayValue)
  }    

  def createITableGlobal(clas: Class,
                         ivtableMap: Map[Symbol, Either[List[Symbol], Symbol]],
                         module: Module): Module =
  {
    def createIVTablePtr(interfaceName: Symbol): Value = {
      BitCastValue(DefinedValue(ivtableGlobalName(clas.name, interfaceName),
                                PointerType(StructType(vtableStructName(interfaceName)))),
                   PointerType(IntType(8)))
    }

    val itableEntries = ivtableMap.toList.map { p =>
      val (interfaceName, ivtable) = p
      val interfaceInfoValue = DefinedValue(interfaceInfoGlobalName(interfaceName),
                                            PointerType(StructType(interfaceInfoStructName)))
      val ivtablePtrValue = ivtable match {
        case Left(_) => createIVTablePtr(interfaceName)
        case Right(otherInterfaceName) => createIVTablePtr(otherInterfaceName)
      }
      StructValue(itableEntryStructName, List(interfaceInfoValue, ivtablePtrValue))
    }
    val itableValue = ArrayValue(StructType(itableEntryStructName), itableEntries)
    val itableGlobal = Global(itableGlobalName(clas.name), 
                              itableValue.ty,
                              Some(itableValue))
    module.add(itableGlobal)
  }

  def convertFunctions(module: Module): Module = {
    var m = module
    for (f <- module.definitions.values.collect { case f: Function => f }) {
      val convertedFunction = f.copyWith("typeParameters" -> Nil)
      m = m.replace(convertedFunction)
    }
    m
  }

  def convertInstructions(module: Module): Module = {
    var m = module
    for (b <- module.definitions.values.collect { case b: Block => b }) {
      val oldInstructions = m.getInstructions(b.instructions)
      val newInstructions = oldInstructions.flatMap(convertInstruction(_, m))
      val newBlock = b.copy(instructions = newInstructions.map(_.name))
      m = m.replace(newBlock).replace(newInstructions: _*)
    }
    m
  }

  def convertInstruction(instruction: Instruction, module: Module): List[Instruction] = {
    instruction match {
      case ptrElemInst: PointerElementInstruction if ptrElemInst.base.ty.isInstanceOf[ClassType] =>
        convertElementInstruction(ptrElemInst, module)
      case newInst: NewInstruction => convertNewInstruction(newInst, module)
      case pcallInst: PointerCallInstruction => convertPCallInstruction(pcallInst, module)
      case scallInst: StaticCallInstruction => convertSCallInstruction(scallInst, module)
      case vcallInst: VirtualCallInstruction => convertVCallInstruction(vcallInst, module)
      case vlookupInst: VirtualLookupInstruction => convertVLookupInstruction(vlookupInst, module)
      case _ => List(instruction)
    }
  }

  def convertElementInstruction(elemInst: PointerElementInstruction, 
                                module: Module): List[Instruction] = 
  {
    val newIndices = elemInst.indices match {
      case IntValue(fieldIndex, width) :: rest =>
        IntValue(0, width) :: IntValue(fieldIndex + 1, width) :: rest
      case _ => throw new RuntimeException("invalid indices")
    }
    val newInst = elemInst.copyWith(("indices" -> newIndices)).asInstanceOf[Instruction]
    List(newInst)
  }

  def convertNewInstruction(instruction: NewInstruction, module: Module): List[Instruction] = {
    val classType = instruction.ty.asInstanceOf[ClassType]
    val className = classType.definitionName
    val allocInst = HeapAllocateInstruction(instruction.name, 
                                            PointerType(StructType(classStructName(className))),
                                            instruction.annotations)
    val vtableInst = StoreElementInstruction(symbolFactory(instruction.name.name :+ "init$"),
                                             UnitType,
                                             DefinedValue(vtableGlobalName(className),
                                                          PointerType(StructType(vtableStructName(className)))),
                                             allocInst.makeValue,
                                             List(IntValue.word(0, module),
                                                  IntValue.word(0, module)))
    val initInst = StaticCallInstruction(symbolFactory(instruction.name.name :+ "init$"),
                                         UnitType,
                                         instruction.constructorName,
                                         classType.typeArguments ++ instruction.typeArguments, 
                                         allocInst.makeValue :: instruction.arguments,
                                         instruction.annotations)
    List(allocInst, vtableInst, initInst)
  }

  def convertPCallInstruction(instruction: PointerCallInstruction,
                              module: Module): List[Instruction] =
  {
    val targetType = instruction.target.ty.asInstanceOf[FunctionType]
    val (arguments, argCastInsts) = castCallArguments(instruction,
                                                      targetType.parameterTypes,
                                                      instruction.arguments,
                                                      module)
    val convertedInst = instruction.copyWith("typeArguments" -> Nil,
                                             "arguments"     -> arguments)
                                   .asInstanceOf[Instruction]
    val returnInsts = castCallReturn(convertedInst, targetType.returnType, module)
    argCastInsts ++ returnInsts
  }

  def convertSCallInstruction(instruction: StaticCallInstruction,
                              module: Module): List[Instruction] =
  {
    val targetType = module.getFunction(instruction.target).ty(module)
    val (arguments, argumentInsts) = castCallArguments(instruction,
                                                       targetType.parameterTypes,
                                                       instruction.arguments,
                                                       module)
    val convertedInst = instruction.copyWith(("typeArguments" -> Nil),
                                             ("arguments"     -> arguments))
                                   .asInstanceOf[Instruction]
    val returnInsts = castCallReturn(convertedInst, targetType.returnType, module)
    argumentInsts ++ returnInsts
  }

  def convertVCallInstruction(instruction: VirtualCallInstruction, 
                              module: Module): List[Instruction] = 
  {
    val zero = IntValue(0, IntType.wordSize(module))
    val objectType = instruction.target.ty match {
      case ty: ObjectType => ty.getEffectiveType(module)
      case _ => throw new RuntimeException("unsupported type: " + instruction.target.ty)
    }
    val defnName = objectType.definitionName

    val vtableInstName = symbolFactory(instruction.name + "vtable$")
    val vtableType = PointerType(StructType(vtableStructName(defnName)))
    val vtableInst = objectType match {
      case _: ClassType => {
        LoadElementInstruction(vtableInstName,
                               vtableType,
                               instruction.target,
                               List(zero, zero),
                               instruction.annotations)
      }
      case interfaceType: InterfaceType => {
        // TODO: need to convert defnName to a string without quoting. This may require
        // some mangling since symbols may contain special characters.
        StaticCallInstruction(vtableInstName,
                              vtableType,
                              "tungsten.load_ivtable",
                              Nil,
                              List(instruction.target, StringValue(defnName.toString)),
                              instruction.annotations)
      }
    }

    val vtableStruct = module.getStruct(vtableStructName(defnName))
    val fieldIndex = 2 + instruction.methodIndex
    val methodType = module.getField(vtableStruct.fields(fieldIndex)).ty.asInstanceOf[FunctionType]
    val methodInst = LoadElementInstruction(symbolFactory(instruction.name + "method$"),
                                            methodType,
                                            vtableInst.makeValue,
                                            List(zero, 
                                                 IntValue(fieldIndex, IntType.wordSize(module))),
                                            instruction.annotations)

    val (arguments, argCastInsts) = castCallArguments(instruction,
                                                      methodType.parameterTypes,
                                                      instruction.target :: instruction.arguments,
                                                      module)

    val typeArguments = objectType.typeArguments ++ instruction.typeArguments
    val callInst = PointerCallInstruction(instruction.name,
                                          instruction.ty,
                                          methodInst.makeValue,
                                          Nil,
                                          arguments,
                                          instruction.annotations)

    val retCastInsts = castCallReturn(callInst, methodType.returnType, module)

    vtableInst :: methodInst :: (argCastInsts ++ retCastInsts)
  }

  def convertVLookupInstruction(instruction: VirtualLookupInstruction,
                                module: Module): List[Instruction] =
  {
    val zero = IntValue(0, IntType.wordSize(module))
    val objectType = instruction.obj.ty.asInstanceOf[ObjectDefinitionType]
    val defnName = objectType.definitionName

    val vtableInstName = symbolFactory(instruction.name + "vtable$")
    val vtableType = PointerType(StructType(vtableStructName(defnName)))
    val vtableInst = objectType match {
      case _: ClassType => {
        LoadElementInstruction(vtableInstName,
                               vtableType,
                               instruction.obj,
                               List(zero, zero),
                               instruction.annotations)
      }
      case interfaceType: InterfaceType => {
        // TODO: need to convert defnName to a string without quoting. This may require
        // some mangling since symbols may contain special characters.
        StaticCallInstruction(vtableInstName,
                              vtableType,
                              "tungsten.load_ivtable",
                              Nil,
                              List(instruction.obj, StringValue(defnName.toString)),
                              instruction.annotations)
      }
    }

    val vtableStruct = module.getStruct(vtableStructName(defnName))
    val fieldIndex = 2 + instruction.methodIndex
    val methodType = module.getField(vtableStruct.fields(fieldIndex)).ty.asInstanceOf[FunctionType]
    val methodInst = LoadElementInstruction(instruction.name,
                                            methodType,
                                            vtableInst.makeValue,
                                            List(zero, 
                                                 IntValue(fieldIndex, IntType.wordSize(module))),
                                            instruction.annotations)

    List(vtableInst, methodInst)
  }

  def castCallArguments(instruction: Instruction,
                        parameterTypes: List[Type], 
                        arguments: List[Value],
                        module: Module): (List[Value], List[Instruction]) = 
  {
    def castNext(parameterTypes: List[Type],
                 arguments: List[Value],
                 castArguments: List[Value],
                 castInstructions: List[Instruction]): (List[Value], List[Instruction]) =
    {
      (parameterTypes, arguments) match {
        case (paramType :: pts, arg :: as) if paramType != arg.ty => {
          val castInst = BitCastInstruction(symbolFactory(instruction.name + "cast$"),
                                            paramType,
                                            arg,
                                            instruction.annotations)
          castNext(pts, as, castInst.makeValue :: castArguments, castInst :: castInstructions)
        }
        case (_ :: pts, arg :: as) =>
          castNext(pts, as, arg :: castArguments, castInstructions)
        case (Nil, Nil) => (castArguments.reverse, castInstructions.reverse)
        case _ => throw new RuntimeException("argument and parameter lists are different lengths")
      }
    }
    castNext(parameterTypes, arguments, Nil, Nil)
  }

  def castCallReturn(callInst: Instruction,
                     returnType: Type,
                     module: Module): List[Instruction] = 
  {
    if (returnType == callInst.ty)
      List(callInst)
    else {
      val origInst = callInst.copyWith("name" -> symbolFactory(callInst.name + "cast$"),
                                       "ty" -> returnType)
                             .asInstanceOf[Instruction]
      val castInst = BitCastInstruction(callInst.name,
                                        callInst.ty,
                                        origInst.makeValue)
      List(origInst, castInst)
    }
  }

  def substituteTypes(interfaceBaseClassNames: Map[Symbol, Symbol], module: Module): Module = {
    module.mapTypes(substituteType(_, interfaceBaseClassNames, module))
  }

  def substituteType(ty: Type, interfaceBaseClassNames: Map[Symbol, Symbol], module: Module): Type = {
    ty match {
      case ClassType(className, _) => 
        PointerType(StructType(classStructName(className)))
      case InterfaceType(interfaceName, _) => {
        val className = interfaceBaseClassNames(interfaceName)
        PointerType(StructType(classStructName(className)))
      }
      case vty: VariableType => 
        substituteType(vty.getEffectiveType(module), interfaceBaseClassNames, module)
      case FunctionType(returnType, _, parameterTypes) => 
        FunctionType(returnType, Nil, parameterTypes)
      case _ => ty
    }
  }

  def removeDefinitions(module: Module): Module = {
    val newDefinitions = module.definitions.filterNot { case (_, defn) =>
      defn.isInstanceOf[Class]         ||
      defn.isInstanceOf[Interface]     ||
      defn.isInstanceOf[TypeParameter]
    }
    module.copyWith(definitions = newDefinitions)
  }  

  val arrayStructName = symbolFromString("tungsten.array")
  val classInfoStructName = symbolFromString("tungsten.class_info")
  val interfaceInfoStructName = symbolFromString("tungsten.interface_info")

  def classNameName(className: Symbol): Symbol = className + "name$"
  def classStructName(className: Symbol): Symbol = className + "data$"
  def classInfoGlobalName(className: Symbol): Symbol = className + "info$"
  def vtableStructName(className: Symbol): Symbol = className + "vtable_type$"
  def vtableGlobalName(className: Symbol): Symbol = className + "vtable$"
  def vtablePtrName(className: Symbol): Symbol = classStructName(className) + "vtable_ptr$"
  def vtableClassInfoName(vtableName: Symbol): Symbol = vtableName + "info$"
  def vtableITableArrayName(vtableName: Symbol): Symbol = vtableName + "itable$"
  def vtableFieldName(vtableName: Symbol, methodName: Symbol, methodIndex: Int): Symbol = {
    new Symbol(vtableName.name :+ methodName.name.last, methodIndex)
  }

  def ivtableGlobalName(className: Symbol, interfaceName: Symbol): Symbol = {
    val fullName = className.name ++ ("ivtable$" :: interfaceName.name)
    new Symbol(fullName, className.id)
  }

  def interfaceNameName(interfaceName: Symbol): Symbol = interfaceName + "name$"
  def interfaceInfoGlobalName(interfaceName: Symbol): Symbol = interfaceName + "info$"
  def itableGlobalName(className: Symbol): Symbol = className + "itable$"
  def itableArrayName(vtableName: Symbol): Symbol = vtableName + "itable$"
  val itableEntryStructName = symbolFromString("tungsten.itable_entry")
}

object LowerPass
  extends Pass
{
  def name = "lower"

  def description = "converts code using higher level features (classes, interfaces, type parameters) to a simpler form"

  def apply(module: Module) = {
    val pass = new LowerPass
    pass(module)
  }
}
