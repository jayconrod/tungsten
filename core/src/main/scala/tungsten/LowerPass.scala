package tungsten

import Utilities._

class LowerPass
  extends Function1[Module, Module]
{
  val symbolFactory = new SymbolFactory

  def apply(module: Module) = {
    var m = module
    m = convertClassesAndInterfaces(m)
    m = convertInstructions(m)
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
    for (i <- interfaces)
      m = m.remove(i.name)
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
    m = replaceClassWithStruct(clas, m)
    m
  }

  def replaceClassWithStruct(clas: Class, module: Module): Module = {
    val vtableField = Field(vtablePtrName(clas.name),
                            PointerType(StructType(vtableStructName(clas.name))))
    val structFieldNames = vtableField.name :: clas.fields
    val struct = Struct(clas.name, structFieldNames)
    module.add(vtableField).replace(struct)
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
    val itableField = Field(itablePtrName(vtableName), PointerType(itableType))
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
      case elemInst: ElementInstruction => convertElementInstruction(elemInst, module)
      case newInst: NewInstruction => convertNewInstruction(newInst, module)
      case vcallInst: VirtualCallInstruction => convertVCallInstruction(vcallInst, module)
      case _ => List(instruction)
    }
  }

  def convertElementInstruction(elemInst: ElementInstruction, module: Module): List[Instruction] = {
    val newIndices = elemInst.indices match {
      case i :: IntValue(fieldIndex, width) :: rest =>
        i :: IntValue(fieldIndex + 1, width) :: rest
      case indices => indices
    }
    val newInst = elemInst.copyWith(("indices" -> newIndices)).asInstanceOf[Instruction]
    List(newInst)
  }

  def convertNewInstruction(instruction: NewInstruction, module: Module): List[Instruction] = {
    val classType = instruction.ty.asInstanceOf[ClassType]
    val className = classType.definitionName
    val struct = module.getStruct(className)
    val allocInst = HeapAllocateInstruction(instruction.name, 
                                            PointerType(StructType(className)),
                                            instruction.annotations)
    val initInst = StaticCallInstruction(symbolFactory(instruction.name.name :+ "_init"),
                                         UnitType,
                                         instruction.constructorName,
                                         classType.typeArguments ++ instruction.typeArguments, 
                                         allocInst.makeValue :: instruction.arguments,
                                         instruction.annotations)
    List(allocInst, initInst)
  }

  def convertVCallInstruction(instruction: VirtualCallInstruction, 
                              module: Module): List[Instruction] = 
  {
    val zero = IntValue(0, IntType.wordSize(module))
    val targetType = instruction.target.ty match {
      case ty: ObjectType => ty.getEffectiveType(module)
      case _ => throw new RuntimeException("unsupported type: " + instruction.target.ty)
    }
    val defnName = targetType.definitionName

    val vtableInstName = symbolFactory(instruction.name + "_vtable")
    val vtableType = PointerType(StructType(vtableStructName(defnName)))
    val vtableInst = targetType match {
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
    val methodType = module.getField(vtableStruct.fields(2 + instruction.methodIndex)).ty
    val methodInst = LoadElementInstruction(symbolFactory(instruction.name + "_method"),
                                            methodType,
                                            vtableInst.makeValue,
                                            List(zero, 
                                                 IntValue(fieldIndex, IntType.wordSize(module))),
                                            instruction.annotations)

    val typeArguments = targetType.typeArguments ++ instruction.typeArguments
    val callInst = PointerCallInstruction(instruction.name,
                                          instruction.ty,
                                          methodInst.makeValue,
                                          typeArguments,
                                          instruction.target :: instruction.arguments,
                                          instruction.annotations)
    List(vtableInst, methodInst, callInst)
  }

  def vtableStructName(className: Symbol): Symbol = className + "_vtable_type"
  def vtableGlobalName(className: Symbol): Symbol = className + "_vtable"
  def vtablePtrName(className: Symbol): Symbol = className + "_vtable_ptr"
  def vtableFieldName(vtableName: Symbol, methodName: Symbol, methodIndex: Int): Symbol = {
    new Symbol(vtableName.name :+ methodName.name.last, methodIndex)
  }

  def ivtableGlobalName(className: Symbol, interfaceName: Symbol): Symbol = {
    val fullName = className.name ++ ("_ivtable" :: interfaceName.name)
    new Symbol(fullName, className.id)
  }

  def itableGlobalName(className: Symbol): Symbol = className + "_itable"    
  def itablePtrName(vtableName: Symbol): Symbol = vtableName + "_itable_ptr"
  def itableSizeName(vtableName: Symbol): Symbol = vtableName + "_itable_size"
  val itableEntryStructName = symbolFromString("tungsten._itable_entry")
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
