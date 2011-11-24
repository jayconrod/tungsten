/** Copyright 2009-2011 Jay Conrod
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

import scala.collection.immutable.TreeMap
import Utilities._

class LowerPass
  extends Pass
{
  import LowerPass._

  def name = "lower"

  def description = "converts code using higher level features (classes, interfaces, type parameters) to a simpler form"

  val lowerInstructions = new LowerInstructionsPass

  def processModule(module: Module) = {
    val interfaceBaseClassNames = findInterfaceBaseClassNames(module)
    var m = module
    m = addDefinitions(m)
    m = convertClassesAndInterfaces(m)
    m = lowerInstructions(m)
    m = eliminateTypeParameters(m)
    m = substituteTypes(interfaceBaseClassNames, m)
    m = removeDefinitions(m)
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
    m = createTypeParameterInfo(interface, module)
    m = createInterfaceInfo(interface, m)
    m = createVTableStruct(interface, m)
    m
  }

  def convertClass(clas: Class, 
                   ivtableMap: Map[Symbol, Either[List[Symbol], Symbol]],
                   module: Module): Module = 
  {
    var m = module
    m = createTypeParameterInfo(clas, module)
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

    val infoNameValue = StructValue(arrayStructName,
                                    List(BitCastValue(nameGlobal.makeValue,
                                                      PointerType(IntType(8))),
                                         IntValue.word(nameValue.elements.size, 
                                                       module)))

    val superclassType = PointerType(StructType(classInfoStructName), ReferenceType.NULLABLE)
    val superclassValue = clas.superclass match {
      case Some(ClassType(superclassName, _, _)) => {
        val rawValue = DefinedValue(classInfoGlobalName(superclassName),
                                    superclassType.clearPointerFlags(ReferenceType.NULLABLE))
        BitCastValue(rawValue, superclassType)
      }
      case None => BitCastValue(NullValue, superclassType)
    }

    val infoValue = StructValue(classInfoStructName,
                                List(infoNameValue,
                                     superclassValue,
                                     typeParameterInfoValue(clas, module)))
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

    val infoNameValue = StructValue(arrayStructName,
                                    List(BitCastValue(nameGlobal.makeValue,
                                                      PointerType(IntType(8))),
                                         IntValue.word(nameValue.elements.size,
                                                       module)))

    val infoValue = StructValue(interfaceInfoStructName,
                                List(infoNameValue,
                                     typeParameterInfoValue(interface, module)))
    val infoGlobal = Global(interfaceInfoGlobalName(interface.name),
                            infoValue.ty,
                            Some(infoValue))
    module.add(nameGlobal, infoGlobal)
  }

  def createTypeParameterInfo(defn: ObjectDefinition, module: Module): Module = {
    if (defn.typeParameters.isEmpty)
      module
    else {
      val typeParameters = module.getTypeParameters(defn.typeParameters)
      val (nameGlobals, infoValues) = typeParameters.zipWithIndex.map({ ind =>
        val (tp, i) = ind
        val nameName = typeParameterNameName(defn.name, i)
        val nameValue = ArrayValue.fromString(tp.name.toString)
        val nameGlobal = new Global(nameName, nameValue.ty, Some(nameValue))

        val flagsValue = IntValue.word(tp.variance.code, module)
        val rawNameValue = BitCastValue(nameGlobal.makeValue, PointerType(IntType(8)))
        val arrayNameValue = StructValue(arrayStructName,
                                         List(rawNameValue, 
                                              IntValue.word(nameValue.elements.size, module)))
        val infoValue = StructValue(typeParameterInfoStructName, List(flagsValue, arrayNameValue))
        (nameGlobal, infoValue)
      }).unzip
      val infoGlobalValue = ArrayValue(StructType(typeParameterInfoStructName), infoValues)
      val infoGlobal = new Global(typeParameterInfoGlobalName(defn.name),
                                  infoGlobalValue.ty,
                                  Some(infoGlobalValue))
      module.add(infoGlobal).add(nameGlobals: _*)
    }
  }

  def typeParameterInfoValue(defn: ObjectDefinition, module: Module): Value = {
    if (defn.typeParameters.isEmpty) {
      StructValue(arrayStructName,
                  List(BitCastValue(NullValue, PointerType(IntType(8))),
                       IntValue.word(0, module)))
    } else {
      val size = defn.typeParameters.size
      val arrayType = PointerType(ArrayType(size, StructType(typeParameterInfoStructName)))
      val array = DefinedValue(typeParameterInfoGlobalName(defn.name), arrayType)
      StructValue(arrayStructName,
                  List(BitCastValue(array, PointerType(IntType(8))),
                       IntValue.word(size, module)))
    }
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

  def eliminateTypeParameters(module: Module): Module = {
    var m = module
    for (f <- module.definitions.values.collect { case f: Function => f })
      m = m.replace(f.copy(typeParameters = Nil))
    m
  }

  def substituteTypes(interfaceBaseClassNames: Map[Symbol, Symbol], module: Module): Module = {
    val newDefinitions = module.definitions.mapValues { defn =>
      if (defnIsRemovable(defn))
        defn
      else
        defn.mapTypes(substituteType(_, interfaceBaseClassNames, module))
    }
    module.copyWith(definitions = newDefinitions)
  }

  def substituteType(ty: Type, interfaceBaseClassNames: Map[Symbol, Symbol], module: Module): Type = {
    ty match {
      case ClassType(className, _, pointerFlags) => 
        PointerType(StructType(classStructName(className)), pointerFlags)
      case InterfaceType(interfaceName, _, pointerFlags) => {
        val className = interfaceBaseClassNames(interfaceName)
        PointerType(StructType(classStructName(className)), pointerFlags)
      }
      case vty: VariableType => 
        substituteType(vty.getUpperBoundType(module).setPointerFlags(vty.pointerFlags), 
                       interfaceBaseClassNames, module)
      case NothingType(pointerFlags) => PointerType(IntType(8), pointerFlags)
      case FunctionType(returnType, _, parameterTypes) => 
        FunctionType(returnType, Nil, parameterTypes)
      case _ => ty
    }
  }

  def removeDefinitions(module: Module): Module = {
    val newDefinitions = module.definitions.filterNot { kv => defnIsRemovable(kv._2) }
    module.copyWith(definitions = newDefinitions)
  }  

  def defnIsRemovable(defn: Definition): Boolean = {
    defn.isInstanceOf[Class]         ||
    defn.isInstanceOf[Interface]     ||
    defn.isInstanceOf[TypeParameter]
  }
}

object LowerPass {
  def apply(module: Module): Module = {
    val pass = new LowerPass
    pass(module)
  }

  val arrayStructName = symbolFromString("tungsten.array")
  val classInfoStructName = symbolFromString("tungsten.class_info")
  val interfaceInfoStructName = symbolFromString("tungsten.interface_info")
  val typeParameterInfoStructName = symbolFromString("tungsten.type_parameter_info")

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

  def typeParameterInfoGlobalName(defnName: Symbol): Symbol = defnName + "params$"
  def typeParameterNameName(defnName: Symbol, index: Int): Symbol = {
    (defnName + "param_names$").copy(id = index)
  }
}

class LowerInstructionsPass
  extends InstructionRewritePass
  with InternalPass
{
  import LowerPass._

  def rewriteInstruction(instruction: Instruction,
                         block: Block,
                         module: Module): RewriteResult =
  {
    instruction match {
      case ptrElemInst: PointerElementInstruction if ptrElemInst.base.ty.isInstanceOf[ClassType] =>
        convertElementInstruction(ptrElemInst, module)
      case newInst: NewInstruction =>
        convertNewInstruction(newInst, module)
      case nullcheckInst: NullCheckInstruction =>
        convertNullCheckInstruction(nullcheckInst, block, module)
      case pcallInst: PointerCallInstruction => 
        convertPCallInstruction(pcallInst, module)
      case scallInst: StaticCallInstruction => 
        convertSCallInstruction(scallInst, module)
      case upcastInst: UpcastInstruction =>
        convertUpcastInstruction(upcastInst, module)
      case vcallInst: VirtualCallInstruction => 
        convertVCallInstruction(vcallInst, module)
      case vlookupInst: VirtualLookupInstruction => 
        convertVLookupInstruction(vlookupInst, module)
      case _ => RewrittenInstructions(List(instruction))
    }
  }

  def convertElementInstruction(elemInst: PointerElementInstruction, 
                                module: Module): RewriteResult = 
  {
    val newIndices = elemInst.indices match {
      case IntValue(fieldIndex, width) :: rest =>
        IntValue(0, width) :: IntValue(fieldIndex + 1, width) :: rest
      case _ => throw new RuntimeException("invalid indices")
    }
    val newInst = elemInst.copyWith(("indices" -> newIndices)).asInstanceOf[Instruction]
    RewrittenInstructions(List(newInst))
  }

  def convertNewInstruction(newInst: NewInstruction, module: Module): RewriteResult = {
    val classType = newInst.ty.asInstanceOf[ClassType]
    val className = classType.definitionName
    val clas = module.getClass(className)

    // Every object has extra words at the end corresponding to its type arguments.
    // The number of words corresponds to the number of type arguments, including
    // sub-arguments. For example if we are constructing A[B[C, D], E[F, G]], then
    // we would need 6 additional words for: B, C, D, E, F, G. Objects with no type
    // arguments require no extra data.
    def countTypeArgumentWords(tyArgs: List[Type], count: Long = 0): Long = {
      tyArgs match {
        case Nil => count
        case (h: ObjectDefinitionType) :: t => {
          val newCount = countTypeArgumentWords(h.typeArguments, count + 1)
          countTypeArgumentWords(t, newCount)
        }
        case _ => throw new RuntimeException("invalid type argument")
      }
    }

    val wordSize = IntType.wordSize(module)/8
    val typeArgWords = countTypeArgumentWords(classType.typeArguments)
    val vtableWord = 1
    val objectSize = clas.size(module)
    val totalSize = align(objectSize, wordSize) + (typeArgWords + vtableWord) * wordSize

    val allocInst = HeapAllocateArrayInstruction(symbolFactory(newInst.name + "new$"),
                                                 PointerType(IntType(8)),
                                                 IntValue.word(totalSize, module))
    
    // We first store a pointer to the object's vtable. This allows us to call methods
    // on the object, so we have to do it before we call the constructor.
    val vtBitcastInst = BitCastInstruction(newInst.name,
                                           PointerType(StructType(classStructName(className))),
                                           allocInst.makeValue)
    val vtableInst = StoreElementInstruction(symbolFactory(newInst.name +"new$"),
                                             UnitType,
                                             DefinedValue(vtableGlobalName(className),
                                                          PointerType(StructType(vtableStructName(className)))),
                                             vtBitcastInst.makeValue,
                                             List(IntValue.word(0, module),
                                                  IntValue.word(0, module)))

    // We need to store a pointer to the class or interface info for each type
    // argument at the end of the object.
    val classInfoInst = BitCastInstruction(symbolFactory(newInst.name + "new$"),
                                           PointerType(PointerType(StructType(classInfoStructName))),
                                           allocInst.makeValue)
    val interfaceInfoInst = BitCastInstruction(symbolFactory(newInst.name + "new$"),
                                               PointerType(PointerType(StructType(interfaceInfoStructName))),
                                               allocInst.makeValue)
    val typeArgumentOffset = align(objectSize + wordSize, wordSize)/wordSize
    def storeTypeArguments(tyArgs: List[Type],
                           insts: List[Instruction] = Nil): List[Instruction] = 
    {
      tyArgs match {
        case Nil => insts
        case (h: ObjectDefinitionType) :: t => {
          val offset = typeArgumentOffset + insts.size
          val storeInst = if (h.isInstanceOf[ClassType]) {
            StoreElementInstruction(symbolFactory(newInst.name + "new$"),
                                    UnitType,
                                    DefinedValue(classInfoGlobalName(h.definitionName),
                                                 PointerType(StructType(classInfoStructName))),
                                    classInfoInst.makeValue,
                                    List(IntValue.word(offset, module)))
          } else if (h.isInstanceOf[InterfaceType]) {
            StoreElementInstruction(symbolFactory(newInst.name + "new$"),
                                    UnitType,
                                    DefinedValue(interfaceInfoGlobalName(h.definitionName),
                                                 PointerType(StructType(interfaceInfoStructName))),
                                    interfaceInfoInst.makeValue,
                                    List(IntValue.word(offset, module)))
          } else {
            throw new RuntimeException("invalid argument type")
          }

          val newInsts = storeTypeArguments(h.typeArguments, storeInst :: insts)
          storeTypeArguments(t, newInsts)
        }
        case _ => throw new RuntimeException("invalid argument type")
      }
    }
    val storeInsts = storeTypeArguments(classType.typeArguments)
    
    // Now the object is initialized, and we can actually call the constructor
    val initInst = StaticCallInstruction(symbolFactory(newInst.name + "new$"),
                                         UnitType,
                                         newInst.constructorName,
                                         Nil,
                                         vtBitcastInst.makeValue :: newInst.arguments)
    val rewritten = allocInst :: vtBitcastInst :: vtableInst :: 
      classInfoInst :: interfaceInfoInst :: (storeInsts.reverse :+ initInst)
    RewrittenInstructions(rewritten)
  }

  def convertNullCheckInstruction(nullcheckInst: NullCheckInstruction,
                                  block: Block,
                                  module: Module): SplitBlock =
  {
    // Create a new error handling block. We create a new one for every nullcheck
    // since different nullchecks may have different exception handlers. Hopefully,
    // we can optimize out redundant ones later.
    val exnInst = NewInstruction(symbolFactory(nullcheckInst.name + "exn$"),
                                 ClassType("tungsten.NullPointerException"),
                                 "tungsten.NullPointerException.ctor",
                                 Nil, Nil)
    val RewrittenInstructions(cExnInsts) = convertNewInstruction(exnInst, module)
    val throwInst = ThrowInstruction(symbolFactory(nullcheckInst.name + "throw$"),
                                     UnitType,
                                     exnInst.makeValue)
    val errorInsts = cExnInsts :+ throwInst

    val catchArguments = block.catchBlock.map(_._2).getOrElse(Nil)
    val catchLiveInParameters = catchArguments flatMap { arg =>
      arg.getSymbols.filter(block.parameters.contains _).map(module.getParameter _)
    }
    val errorBlockParameters = catchLiveInParameters.map { p =>
      p.copy(name = symbolFactory(p.name))
    }
    val errorBlockArguments = catchLiveInParameters.map(_.makeValue)
    val errorBlockSubstitutions =
     (catchLiveInParameters.map(_.name) zip errorBlockParameters.map(_.name)).toMap
    val errorBlockCatchArguments = catchArguments.map(_.substitute(catchLiveInParameters.map(_.name),
                                                                   errorBlockParameters.map(_.name)))
    val errorBlockCatchBlock = block.catchBlock.map(_.copy(_2 = errorBlockCatchArguments))

    val errorBlock = Block(symbolFactory(nullcheckInst.name + "npebb$"),
                           errorBlockParameters.map(_.name),
                           errorInsts.map(_.name),
                           errorBlockCatchBlock,
                           block.annotations)
    val errorModule = module.add(errorBlock)
                            .add(errorInsts: _*)
                            .add(errorBlockParameters: _*)

    // Split the block we were processing. The instructions we have already processed
    // will form a new block, and any live-in values will become parameters.
    SplitBlock(Nil, List(errorBlock), errorModule,
               { (splitBlockName, splitBlockArguments, splitModule) =>
      // The actual checking code goes at the end of the new block
      val castInst = BitCastInstruction(nullcheckInst.name,
                                        nullcheckInst.ty,
                                        nullcheckInst.value,
                                        nullcheckInst.annotations)
      val nullValue = BitCastValue(NullValue, nullcheckInst.value.ty)
      val checkInst = RelationalOperatorInstruction(symbolFactory(nullcheckInst.name + "cmp$"),
                                                    BooleanType,
                                                    RelationalOperator.EQUAL,
                                                    nullcheckInst.value,
                                                    nullValue)
      val branchInst = ConditionalBranchInstruction(symbolFactory(nullcheckInst.name + "cond$"),
                                                    UnitType,
                                                    checkInst.makeValue,
                                                    errorBlock.name,
                                                    errorBlockArguments,
                                                    splitBlockName,
                                                    splitBlockArguments)
      RewrittenInstructions(List(castInst, checkInst, branchInst))
    })
  }

  def convertPCallInstruction(pcallInst: PointerCallInstruction,
                              module: Module): RewriteResult =
  {
    val targetType = pcallInst.target.ty.asInstanceOf[FunctionType]
    val (arguments, argCastInsts) = castCallArguments(pcallInst,
                                                      targetType.parameterTypes,
                                                      pcallInst.arguments,
                                                      module)
    val convertedInst = pcallInst.copy(typeArguments = Nil,
                                       arguments     = arguments)
                                 .asInstanceOf[Instruction]
    val returnInsts = castCallReturn(convertedInst, targetType.returnType, module)
    RewrittenInstructions(argCastInsts ++ returnInsts)
  }

  def convertSCallInstruction(scallInstruction: StaticCallInstruction,
                              module: Module): RewriteResult =
  {
    val targetType = module.getFunction(scallInstruction.target).ty(module)
    val (arguments, argumentInsts) = castCallArguments(scallInstruction,
                                                       targetType.parameterTypes,
                                                       scallInstruction.arguments,
                                                       module)
    val convertedInst = scallInstruction.copy(typeArguments = Nil,
                                              arguments     = arguments)
                                        .asInstanceOf[Instruction]
    val returnInsts = castCallReturn(convertedInst, targetType.returnType, module)
    RewrittenInstructions(argumentInsts ++ returnInsts)
  }

  def convertUpcastInstruction(upcastInst: UpcastInstruction,
                               module: Module): RewriteResult =
  {
    val bitcast = BitCastInstruction(upcastInst.name,
                                     upcastInst.ty,
                                     upcastInst.value,
                                     upcastInst.annotations)
    RewrittenInstructions(List(bitcast))
  }

  def convertVCallInstruction(vcallInst: VirtualCallInstruction, 
                              module: Module): RewriteResult = 
  {
    val zero = IntValue(0, IntType.wordSize(module))
    val objectType = vcallInst.target.ty.asInstanceOf[ObjectDefinitionType]
    val isClass = objectType.isInstanceOf[ClassType]
    val defnName = objectType.definitionName

    val vtableInstName = symbolFactory(vcallInst.name + "vtable$")
    val vtableType = PointerType(StructType(LowerPass.vtableStructName(defnName)))
    val vtableInsts = objectType match {
      case _: ClassType => {
        List(LoadElementInstruction(vtableInstName,
                                    vtableType,
                                    vcallInst.target,
                                    List(zero, zero),
                                    vcallInst.annotations))
      }
      case interfaceType: InterfaceType => {
        val infoValue = DefinedValue(interfaceInfoGlobalName(interfaceType.definitionName),
                                     PointerType(StructType(interfaceInfoStructName)))
        val ivtInst = StaticCallInstruction(vtableInstName,
                                            PointerType(IntType(8)),
                                            "tungsten.load_ivtable",
                                            Nil,
                                            List(vcallInst.target, infoValue),
                                            vcallInst.annotations)
        val castInst = BitCastInstruction(symbolFactory(vtableInstName),
                                          vtableType,
                                          ivtInst.makeValue)
        List(ivtInst, castInst)
      }
    }
    val vtableValue = vtableInsts.last.makeValue

    val vtableStruct = module.getStruct(vtableStructName(defnName))
    val fieldIndex = if (isClass) 2 + vcallInst.methodIndex else vcallInst.methodIndex
    val methodType = module.getField(vtableStruct.fields(fieldIndex)).ty.asInstanceOf[FunctionType]
    val methodInst = LoadElementInstruction(symbolFactory(vcallInst.name + "method$"),
                                            methodType,
                                            vtableValue,
                                            List(zero, 
                                                 IntValue(fieldIndex, IntType.wordSize(module))),
                                            vcallInst.annotations)

    val (arguments, argCastInsts) = castCallArguments(vcallInst,
                                                      methodType.parameterTypes,
                                                      vcallInst.target :: vcallInst.arguments,
                                                      module)

    val callInst = PointerCallInstruction(vcallInst.name,
                                          vcallInst.ty,
                                          methodInst.makeValue,
                                          Nil,
                                          arguments,
                                          vcallInst.annotations)

    val retCastInsts = castCallReturn(callInst, methodType.returnType, module)

    RewrittenInstructions(vtableInsts ++ (methodInst :: argCastInsts ++ retCastInsts))
  }

  def convertVLookupInstruction(vlookupInst: VirtualLookupInstruction,
                                module: Module): RewriteResult =
  {
    val zero = IntValue(0, IntType.wordSize(module))
    val objectType = vlookupInst.obj.ty.asInstanceOf[ObjectDefinitionType]
    val isClass = objectType.isInstanceOf[ClassType]
    val defnName = objectType.definitionName

    val vtableInstName = symbolFactory(vlookupInst.name + "vtable$")
    val vtableType = PointerType(StructType(vtableStructName(defnName)))
    val vtableInsts = objectType match {
      case _: ClassType => {
        List(LoadElementInstruction(vtableInstName,
                                    vtableType,
                                    vlookupInst.obj,
                                    List(zero, zero),
                                    vlookupInst.annotations))
      }
      case interfaceType: InterfaceType => {
        val infoValue = DefinedValue(interfaceInfoGlobalName(interfaceType.definitionName),
                                     PointerType(StructType(interfaceInfoStructName)))
        val ivtInst = StaticCallInstruction(vtableInstName,
                                            PointerType(IntType(8)),
                                            "tungsten.load_ivtable",
                                            Nil,
                                            List(vlookupInst.obj, infoValue),
                                            vlookupInst.annotations)
        val castInst = BitCastInstruction(symbolFactory(vtableInstName),
                                          vtableType,
                                          ivtInst.makeValue)
        List(ivtInst, castInst)
      }
    }
    val vtableValue = vtableInsts.last.makeValue

    val vtableStruct = module.getStruct(vtableStructName(defnName))
    val fieldIndex = if (isClass) 2 + vlookupInst.methodIndex else vlookupInst.methodIndex
    val methodType = module.getField(vtableStruct.fields(fieldIndex)).ty.asInstanceOf[FunctionType]
    val methodInst = LoadElementInstruction(vlookupInst.name,
                                            methodType,
                                            vtableValue,
                                            List(zero, 
                                                 IntValue(fieldIndex, IntType.wordSize(module))),
                                            vlookupInst.annotations)

    RewrittenInstructions(vtableInsts :+ methodInst)
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
        case (VariadicType :: Nil, arg :: as) =>
          castNext(parameterTypes, as, arg :: castArguments, castInstructions)
        case (paramType :: pts, arg :: as) if paramType != arg.ty => {
          val castInst = BitCastInstruction(symbolFactory(instruction.name + "cast$"),
                                            paramType,
                                            arg,
                                            instruction.annotations)
          castNext(pts, as, castInst.makeValue :: castArguments, castInst :: castInstructions)
        }
        case (_ :: pts, arg :: as) =>
          castNext(pts, as, arg :: castArguments, castInstructions)
        case (VariadicType :: Nil, Nil) | (Nil, Nil) =>
          (castArguments.reverse, castInstructions.reverse)
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
}

object LowerInstructionsPass {
  def apply(module: Module): Module = {
    val pass = new LowerInstructionsPass
    pass(module)
  }
}
