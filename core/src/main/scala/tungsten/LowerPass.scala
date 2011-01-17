package tungsten

class LowerPass
  extends Function1[Module, Module]
{
  val symbolFactory = new SymbolFactory

  def apply(module: Module) = {
    var m = module
    for (clas <- m.definitions.values collect { case c: Class => c })
      m = convertClass(clas, m)
    m
  }

  def convertClass(clas: Class, module: Module): Module = {
    var m = module
    m = createVtableStruct(clas, m)
    m = createVtableGlobal(clas, m)
    m = replaceClassWithStruct(clas, m)
    m
  }

  def replaceClassWithStruct(clas: Class, module: Module): Module = {
    val vtableField = Field(vtableFieldName(clas.name),
                            PointerType(StructType(vtableStructName(clas.name))))
    val structFieldNames = vtableField.name :: clas.fields
    val struct = Struct(clas.name, structFieldNames)
    module.add(vtableField).replace(struct)
  }

  def createVtableStruct(clas: Class, module: Module): Module = {
    val methods = module.getFunctions(clas.methods)
    val vtableFields = ((0 until methods.size).toList zip methods) map { p =>
      val (methodIndex, method) = p
      val fieldName = vtableFieldName(vtableStructName(clas.name), method.name, methodIndex)
      Field(fieldName, method.ty(module))
    }
    val vtableStruct = Struct(vtableStructName(clas.name), vtableFields.map(_.name))
    module.add(vtableFields: _*).add(vtableStruct)
  }

  def createVtableGlobal(clas: Class, module: Module): Module = {
    val methods = module.getFunctions(clas.methods)
    val methodValues = methods.map { m => DefinedValue(m.name, m.ty(module)) }
    val vtableValue = StructValue(vtableStructName(clas.name), methodValues)
    val vtableGlobal = Global(vtableGlobalName(clas.name),
                              StructType(vtableStructName(clas.name)),
                              Some(vtableValue))
    module.add(vtableGlobal)
  }

  def vtableStructName(className: Symbol): Symbol = className + "_vtable_type"
  def vtableGlobalName(className: Symbol): Symbol = className + "_vtable"
  def vtableFieldName(className: Symbol): Symbol = className + "_vtable_ptr"

  def vtableFieldName(vtableName: Symbol, methodName: Symbol, methodIndex: Int): Symbol = {
    new Symbol(vtableName.name :+ methodName.name.last, methodIndex)
  }
}

object LowerPass
  extends Function1[Module, Module]
{
  def apply(module: Module) = {
    val pass = new LowerPass
    pass(module)
  }
}
