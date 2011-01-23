package tungsten

import Utilities._

class LowerPass
  extends Function1[Module, Module]
{
  val symbolFactory = new SymbolFactory

  def apply(module: Module) = {
    var m = module
    m = convertClassesAndInterfaces(m)
    m
  }

  def convertClassesAndInterfaces(module: Module): Module = {
    val classes = module.definitions.values collect { case c: Class => c }
    val interfaces = module.definitions.values collect { case i: Interface => i }
    val ivtableMaps = classes.map { c => (c.name, c.getIVTables(None, Map(), module)) }.toMap

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
    m = m.remove(interface.name)
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
    m = createVTableGlobal(clas, m)
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

  def createVTableStruct(clas: ObjectDefinition, module: Module): Module = {
    val vtableName = vtableStructName(clas.name)
    val methods = module.getFunctions(clas.methods)
    val methodFields = ((0 until methods.size).toList zip methods) map { p =>
      val (methodIndex, method) = p
      val fieldName = vtableFieldName(vtableName, method.name, methodIndex)
      Field(fieldName, method.ty(module))
    }
    val itableType = module.getGlobal(itableGlobalName(clas.name)).ty
    val itableField = Field(itablePtrName(vtableName), PointerType(itableType))
    val vtableFields = itableField :: methodFields
    val vtableStruct = Struct(vtableName, vtableFields.map(_.name))
    module.add(vtableFields: _*).add(vtableStruct)
  }

  def createVTableGlobal(clas: Class, module: Module): Module = {
    val methods = module.getFunctions(clas.methods)
    val methodValues = methods.map { m => DefinedValue(m.name, m.ty(module)) }
    val itableType = module.getGlobal(itableGlobalName(clas.name)).ty
    val itableValue = DefinedValue(itableGlobalName(clas.name), itableType)
    val vtableValue = StructValue(vtableStructName(clas.name), itableValue :: methodValues)
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
      val methods = module.getFunctions(methodNames)
      val ivtableFields = methods.map { m => DefinedValue(m.name, m.ty(module)) }
      val ivtableValue = StructValue(vtableStructName(interfaceName), ivtableFields)
      Global(ivtableGlobalName(clas.name, interfaceName),
                               StructType(vtableStructName(interfaceName)),
                               Some(ivtableValue))
    }
    module.add(ivtableGlobals: _*)
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
      BitCastValue(DefinedValue(vtableGlobalName(interfaceName),
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
  val itableEntryStructName = symbolFromString("tungsten._itable_entry")
}

object LowerPass
  extends Function1[Module, Module]
{
  def apply(module: Module) = {
    val pass = new LowerPass
    pass(module)
  }
}
