package tungsten

import Utilities._

class LowerPass
  extends Function1[Module, Module]
{
  private var symbolFactory: SymbolFactory = new SymbolFactory

  def apply(module: Module) = {
    symbolFactory = new SymbolFactory(module.highestSymbolId + 1)
    var m = module
    m = convertClassesAndInterfaces(m)
    m = convertInstructions(m)
    m = convertFunctions(m)
    m = substituteTypes(m)
    m = removeDefinitions(m)
    m
  }

  def convertClassesAndInterfaces(module: Module): Module = {
    val classes = module.definitions.values collect { case c: Class => c }
    val interfaces = module.definitions.values collect { case i: Interface => i }
    val ivtableMaps = classes.map { c => (c.name, c.getIVTables(module)) }.toMap

    var m = module
    m = createITableEntryStruct(m)
    for (i <- interfaces)
      m = convertInterface(i, m)
    for (c <- classes)
      m = convertClass(c, ivtableMaps(c.name), m)
    m
  }

  def convertInterface(interface: Interface, module: Module): Module = {
    var m = module
    m = createVTableStruct(interface, module)
    m
  }

  def convertClass(clas: Class, 
                   ivtableMap: Map[Symbol, Either[List[Symbol], Symbol]],
                   module: Module): Module = 
  {
    var m = module
    m = createIVTableGlobals(clas, ivtableMap, m)
    m = createITableGlobal(clas, ivtableMap, m)
    m = createVTableStruct(clas, m)
    m = createVTableGlobal(clas, ivtableMap.size, m)
    m = createClassStruct(clas, m)
    m
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
    val itableType = PointerType(StructType(itableEntryStructName))
    val itableField = Field(itablePtrName(vtableName), itableType)
    val itableSizeField = Field(itableSizeName(vtableName), IntType.wordType(module))
    val vtableFields = itableField :: itableSizeField :: methodFields
    val vtableStruct = Struct(vtableName, vtableFields.map(_.name))
    module.add(vtableFields: _*).add(vtableStruct)
  }

  def createVTableGlobal(clas: Class, itableSize: Int, module: Module): Module = {
    val itableValues = createITableValuesForVTable(clas.name, itableSize, module)
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
    val itableValues = createITableValuesForVTable(clas.name, ivtableMap.size, module)
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
      val ivtableValues = itableValues ++ ivtableMethodValues
      val ivtableValue = StructValue(vtableStructName(interfaceName), ivtableValues)
      Global(ivtableGlobalName(clas.name, interfaceName),
                               StructType(vtableStructName(interfaceName)),
                               Some(ivtableValue))
    }
    module.add(ivtableGlobals: _*)
  }

  def createITableValuesForVTable(className: Symbol, 
                                  itableSize: Int, 
                                  module: Module): List[Value] = 
  {
    val itableGlobalType = PointerType(ArrayType(itableSize, StructType(itableEntryStructName)))
    val itableType = PointerType(StructType(itableEntryStructName))
    val itableGlobalValue = DefinedValue(itableGlobalName(className), itableGlobalType)
    val itableValue = BitCastValue(itableGlobalValue, itableType)

    val itableSizeValue = IntValue(itableSize, IntType.wordSize(module))

    List(itableValue, itableSizeValue)
  }    

  def createITableEntryStruct(module: Module): Module = {
    val nameField = Field(itableEntryStructName + "interfaceName", StringType)
    val ivtablePtrField = Field(itableEntryStructName + "ivtable", 
                                PointerType(IntType(8)))
    val fields = List(nameField, ivtablePtrField)
    val entryStruct = Struct(itableEntryStructName, fields.map(_.name))
    module.add(fields: _*).add(entryStruct)
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
      val interfaceNameValue = StringValue(interfaceName.toString)
      val ivtablePtrValue = ivtable match {
        case Left(_) => createIVTablePtr(interfaceName)
        case Right(otherInterfaceName) => createIVTablePtr(otherInterfaceName)
      }
      StructValue(itableEntryStructName, List(interfaceNameValue, ivtablePtrValue))
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
    val initInst = StaticCallInstruction(symbolFactory(instruction.name.name :+ "init$"),
                                         UnitType,
                                         instruction.constructorName,
                                         classType.typeArguments ++ instruction.typeArguments, 
                                         allocInst.makeValue :: instruction.arguments,
                                         instruction.annotations)
    List(allocInst, initInst)
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

  def substituteTypes(module: Module): Module = {
    module.mapTypes(substituteType(_, module))
  }

  def substituteType(ty: Type, module: Module): Type = {
    ty match {
      case ClassType(className, _) => 
        PointerType(StructType(classStructName(className)))
      case InterfaceType(interfaceName, _) => {
        val interfaceDefn = module.getInterface(interfaceName)
        val classDefn = interfaceDefn.getParentClass(module)
        PointerType(StructType(classStructName(classDefn.name)))
      }
      case vty: VariableType => 
        substituteType(vty.getEffectiveType(module), module)
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

  def classStructName(className: Symbol): Symbol = className + "data$"
  def vtableStructName(className: Symbol): Symbol = className + "vtable_type$"
  def vtableGlobalName(className: Symbol): Symbol = className + "vtable$"
  def vtablePtrName(className: Symbol): Symbol = classStructName(className) + "vtable_ptr$"
  def vtableFieldName(vtableName: Symbol, methodName: Symbol, methodIndex: Int): Symbol = {
    new Symbol(vtableName.name :+ methodName.name.last, methodIndex)
  }

  def ivtableGlobalName(className: Symbol, interfaceName: Symbol): Symbol = {
    val fullName = className.name ++ ("ivtable$" :: interfaceName.name)
    new Symbol(fullName, className.id)
  }

  def itableGlobalName(className: Symbol): Symbol = className + "itable$"
  def itablePtrName(vtableName: Symbol): Symbol = vtableName + "itable_ptr$"
  def itableSizeName(vtableName: Symbol): Symbol = vtableName + "itable_size$"
  val itableEntryStructName = symbolFromString("tungsten.itable_entry$")
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
