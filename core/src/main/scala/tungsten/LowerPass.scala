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
    var m = module

    val flagsValue = IntValue(CLASS_INFO_CLASS_FLAG, 32)

    m = createDefinitionName(clas, m)
    m = createTypeParameterInfo(clas, m)

    val superclassType = PointerType(StructType(classInfoStructName), ReferenceType.NULLABLE)
    val superclassValue = clas.superclass match {
      case Some(ClassType(superclassName, _, _)) => {
        val rawValue = DefinedValue(definitionInfoGlobalName(superclassName),
                                    superclassType.clearPointerFlags(ReferenceType.NULLABLE))
        BitCastValue(rawValue, superclassType)
      }
      case None => BitCastValue(NullValue, superclassType)
    }

    val supertypes = clas.selfType.supertypes(module).tail  // drop self type
    val supertypeCountValue = IntValue.word(supertypes.size, module)

    m = createSupertypeInfo(clas, supertypes, m)
    m = createSupertypeInstructions(clas, supertypes, m)

    val sizeValue = IntValue.word(clas.size(module), module)

    val infoValue = StructValue(classInfoStructName,
                                List(flagsValue,
                                     definitionNameValue(clas, m),
                                     typeParameterInfoValue(clas, m),
                                     superclassValue,
                                     supertypeCountValue,
                                     supertypeInfoValue(clas, m),
                                     supertypeInstructionsValue(clas, m),
                                     sizeValue))
    val infoGlobal = Global(definitionInfoGlobalName(clas.name),
                            infoValue.ty,
                            Some(infoValue))
    m = m.add(infoGlobal)

    m
  }

  def createInterfaceInfo(interface: Interface, module: Module): Module = {
    var m = module

    val flagsValue = IntValue(CLASS_INFO_INTERFACE_FLAG, 32)

    m = createDefinitionName(interface, m)
    m = createTypeParameterInfo(interface, m)

    val supertypeType = PointerType(StructType(classInfoStructName))
    val supertypeValue = interface.supertype match {
      case cty: ClassType =>
        DefinedValue(definitionInfoGlobalName(cty.definitionName), supertypeType)
      case ity: InterfaceType => {
        val rawValue = DefinedValue(definitionInfoGlobalName(ity.definitionName),
                                    PointerType(StructType(interfaceInfoStructName)))
        BitCastValue(rawValue, supertypeType)
      }
    }

    val supertypes = interface.selfType.supertypes(module).tail // drop self type
    val supertypeCountValue = IntValue.word(supertypes.size, module)

    m = createSupertypeInfo(interface, supertypes, m)
    m = createSupertypeInstructions(interface, supertypes, m)

    val infoValue = StructValue(interfaceInfoStructName,
                                List(flagsValue,
                                     definitionNameValue(interface, m),
                                     typeParameterInfoValue(interface, m),
                                     supertypeValue,
                                     supertypeCountValue,
                                     supertypeInfoValue(interface, m),
                                     supertypeInstructionsValue(interface, m)))
    val infoGlobal = Global(definitionInfoGlobalName(interface.name),
                            infoValue.ty,
                            Some(infoValue))
    m = m.add(infoGlobal)

    m
  }

  def createDefinitionName(defn: ObjectDefinition, module: Module): Module = {
    val nameValue = ArrayValue.fromString(defn.name.toString)
    val nameGlobal = Global(definitionNameName(defn.name), nameValue.ty, Some(nameValue))
    module.add(nameGlobal)
  }

  def definitionNameValue(defn: ObjectDefinition, module: Module): Value = {
    val global = module.getGlobal(definitionNameName(defn.name))
    StructValue(arrayStructName,
                List(BitCastValue(global.makeValue, PointerType(IntType(8))),
                     IntValue.word(global.ty.asInstanceOf[ArrayType].length, module)))
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

        val flags = tp.variance match {
          case Variance.INVARIANT => TYPE_PARAMETER_INFO_INVARIANT_FLAG
          case Variance.COVARIANT => TYPE_PARAMETER_INFO_COVARIANT_FLAG
          case Variance.CONTRAVARIANT => TYPE_PARAMETER_INFO_CONTRAVARIANT_FLAG
        }
        val flagsValue = IntValue(flags, 32)
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

  def createSupertypeInfo(defn: ObjectDefinition,
                          supertypes: List[ObjectDefinitionType], 
                          module: Module): Module = 
  {
    if (supertypes.isEmpty)
      module
    else {
      val elemType = PointerType(StructType(classInfoStructName))
      val supertypeValues = supertypes map { ty =>
        ty match {
          case _: ClassType => {
            val defnType = PointerType(StructType(classInfoStructName))
            DefinedValue(definitionInfoGlobalName(ty.definitionName), defnType)
          }
          case _: InterfaceType => {
            val rawType = PointerType(StructType(interfaceInfoStructName))
            val rawValue = DefinedValue(definitionInfoGlobalName(ty.definitionName), rawType)
            val castType = PointerType(StructType(classInfoStructName))
            BitCastValue(rawValue, castType)
          }
        }
      }
      val infoType = ArrayType(supertypes.size, elemType)
      val infoValue = ArrayValue(elemType, supertypeValues)
      val infoGlobal = Global(classSupertypeInfoGlobalName(defn.name), infoType, Some(infoValue))
      module.add(infoGlobal)
    }
  }

  def supertypeInfoValue(defn: ObjectDefinition, module: Module): Value = {
    val infoTypeFlags = if (defn.isInstanceOf[Class]) ReferenceType.NULLABLE else 0
    val infoType = PointerType(PointerType(StructType(classInfoStructName)), infoTypeFlags)
    module.definitions.get(classSupertypeInfoGlobalName(defn.name)) match {
      case None => BitCastValue(NullValue, infoType)
      case Some(g: Global) => BitCastValue(g.makeValue, infoType)
      case _ => throw new RuntimeException("invalid definition")
    }
  }

  def createSupertypeInstructions(defn: ObjectDefinition, 
                                  supertypes: List[ObjectDefinitionType],
                                  module: Module): Module = 
  {
    val tyParamMap = defn.typeParameters.zipWithIndex.toMap

    val instType = PointerType(IntType(8), ReferenceType.NULLABLE)
    def createInstructionList(ty: ObjectDefinitionType,
                              tyArgs: List[ObjectType],
                              instructions: List[Value]): List[Value] = 
    {
      tyArgs match {
        case Nil => instructions
        case tyArg :: rest => {
          val newInsts = tyArg match {
            case _: NothingType =>
              BitCastValue(NullValue, instType) :: instructions
            case v: VariableType => {
              val index = tyParamMap(v.variableName)
              val indexValue = IntValue.word((index << 1) | 1, module)
              BitCastValue(indexValue, instType) :: instructions
            }
            case objectType: ObjectDefinitionType => {
              val infoName = definitionInfoGlobalName(objectType.definitionName)
              val infoStructName = objectType match {
                case _: ClassType => classInfoStructName
                case _: InterfaceType => interfaceInfoStructName
              }
              val infoType = PointerType(StructType(infoStructName))
              val newInst = BitCastValue(DefinedValue(infoName, infoType), instType)
              createInstructionList(objectType, objectType.typeArguments, newInst :: instructions)
            }
          }
          createInstructionList(ty, rest, newInsts)
        }
      }
    }

    val instArrayDefns = supertypes.zipWithIndex map { ind =>
      val (ty, index) = ind
      val instList = createInstructionList(ty, ty.typeArguments, Nil)
      if (instList.isEmpty) {
        None
      } else {
        val lastInst = BitCastValue(IntValue.word(-1, module), instType)
        val instValues = (lastInst :: instList).reverse
        val defnName = classSupertypeInstructionsName(defn.name, index)
        val defnValue = ArrayValue(instType, instValues)
        Some(Global(defnName, defnValue.ty, Some(defnValue)))
      }
    }

    val instArrayArrayDefn = if (!instArrayDefns.exists(_.isDefined)) {
      None
    } else {
      val instArrayArrayName = classSupertypeInstructionArraysName(defn.name)
      val instArrayArrayElemType = PointerType(instType, ReferenceType.NULLABLE)
      val instArrayArrayType = ArrayType(instArrayDefns.size, instArrayArrayElemType)
      val instArrayArrayValue = ArrayValue(instArrayArrayElemType, instArrayDefns map {
        case None => BitCastValue(NullValue, instArrayArrayElemType)
        case Some(defn) => BitCastValue(defn.makeValue, instArrayArrayElemType)
      })
      Some(Global(instArrayArrayName, instArrayArrayType, Some(instArrayArrayValue)))
    }

    val newDefns = (instArrayArrayDefn :: instArrayDefns).flatten
    module.add(newDefns: _*)
  }

  def supertypeInstructionsValue(defn: ObjectDefinition, module: Module): Value = {
    val tyFlags = ReferenceType.NULLABLE
    val ty = PointerType(PointerType(PointerType(IntType(8), tyFlags), tyFlags), tyFlags)
    module.definitions.get(classSupertypeInstructionArraysName(defn.name)) match {
      case None => BitCastValue(NullValue, ty)
      case Some(g: Global) => BitCastValue(g.makeValue, ty)
      case _ => throw new RuntimeException("invalid definition")
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
    val classInfoName = definitionInfoGlobalName(className)
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
      val interfaceInfoValue = DefinedValue(definitionInfoGlobalName(interfaceName),
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

  def definitionInfoGlobalName(defnName: Symbol): Symbol = defnName + "info$"
  def definitionNameName(defnName: Symbol): Symbol = defnName + "name$"
  def classStructName(className: Symbol): Symbol = className + "data$"
  def classSupertypeInfoGlobalName(className: Symbol): Symbol = className + "supertype_info$"
  def classSupertypeInstructionArraysName(className: Symbol): Symbol = 
    className + "supertype_instruction_arrays$"
  def classSupertypeInstructionsName(className: Symbol, index: Int): Symbol =
    className + "supertype_instructions$" + index
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

  def itableGlobalName(className: Symbol): Symbol = className + "itable$"
  def itableArrayName(vtableName: Symbol): Symbol = vtableName + "itable$"
  val itableEntryStructName = symbolFromString("tungsten.itable_entry")

  def typeParameterInfoGlobalName(defnName: Symbol): Symbol = defnName + "type_parameter_info$"
  def typeParameterNameName(defnName: Symbol, index: Int): Symbol = {
    (defnName + "type_parameter_names$").copy(id = index)
  }

  val CLASS_INFO_KIND_MASK = 0x1
  val CLASS_INFO_CLASS_FLAG = 0x1
  val CLASS_INFO_INTERFACE_FLAG = 0x0

  val TYPE_PARAMETER_INFO_VARIANCE_MASK = 0x3
  val TYPE_PARAMETER_INFO_INVARIANT_FLAG = 0x0
  val TYPE_PARAMETER_INFO_COVARIANT_FLAG = 0x1
  val TYPE_PARAMETER_INFO_CONTRAVARIANT_FLAG = 0x2
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
      case checkedcastInst: CheckedCastInstruction =>
        convertCheckedCastInstruction(checkedcastInst, block, module)
      case instanceofInst: InstanceOfInstruction =>
        convertInstanceOfInstruction(instanceofInst, module)
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

  def countTypeArgumentWords(typeArgs: List[ObjectType]): Long = {
    def count(typeArgs: List[ObjectType], n: Long): Long = {
      typeArgs match {
        case Nil => n
        case (t: ObjectDefinitionType) :: ts => {
          val nn = count(t.typeArguments, n + 1)
          count(ts, nn)
        }
        case (t: NothingType) :: ts => count(ts, n + 1)          
        case _ => throw new RuntimeException("invalid type argument")
      }
    }
    count(typeArgs, 0)
  }

  def storeTypeArgumentWords(base: Value,
                             offset: Long,
                             typeArgs: List[ObjectType],
                             makeName: () => Symbol,
                             module: Module): List[Instruction] =
  {
    def store(typeArgs: List[ObjectType],
              insts: List[Instruction]): List[Instruction] =
    {
      typeArgs match {
        case Nil => insts
        case t :: ts => {
          val storeOffset = offset + insts.size
          val storeType = PointerType(StructType(LowerPass.classInfoStructName))
          val storeValue = t match {
            case ct: ClassType =>
              DefinedValue(LowerPass.definitionInfoGlobalName(ct.definitionName), storeType)
            case it: InterfaceType => {
              val rawValue = DefinedValue(LowerPass.definitionInfoGlobalName(it.definitionName),
                                          PointerType(StructType(LowerPass.interfaceInfoStructName)))
              BitCastValue(rawValue, storeType)
            }
            case _: NothingType =>
              BitCastValue(NullValue, storeType)
            case _ => throw new RuntimeException("invalid type")
          }
          val storeInst = StoreElementInstruction(makeName(), UnitType,
                                                  storeValue, base,
                                                  List(IntValue.word(storeOffset, module)))

          val typeArgs = t match {
            case oty: ObjectDefinitionType => oty.typeArguments
            case _ => Nil
          }
          val newInsts = store(typeArgs, storeInst :: insts)
          store(ts, newInsts)
        }
      }
    }
    store(typeArgs, Nil).reverse
  }

  def createErrorBlock(newName: () => Symbol,
                       errorClassName: Symbol,
                       errorCtorName: Symbol,
                       errorArguments: List[Value],
                       block: Block,
                       module: Module): (Block, List[Value], Module) =
  {
    val newInst = NewInstruction(newName(),
                                 ClassType(errorClassName),
                                 errorCtorName,
                                 Nil, errorArguments)
    val loweredNewInsts = convertNewInstruction(newInst, module).rewritten
    val throwInst = ThrowInstruction(newName(), UnitType, newInst.makeValue)
    val errorInsts = loweredNewInsts :+ throwInst

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

    val errorBlock = Block(newName() + "error$",
                           errorBlockParameters.map(_.name),
                           errorInsts.map(_.name),
                           errorBlockCatchBlock,
                           block.annotations)
    val errorModule = module.add(errorBlock)
                            .add(errorInsts: _*)
                            .add(errorBlockParameters: _*)
    (errorBlock, errorBlockArguments, errorModule)
  }

  def convertCheckedCastInstruction(checkedcastInst: CheckedCastInstruction,
                                    block: Block,
                                    module: Module): SplitBlock =
  {
    def newName = symbolFactory(checkedcastInst.name + "cast$")

    // Create a new error handling block. We create a new one for every checked cast
    // since different casts may have different exception handlers. Maybe we can 
    // optimize redundant ones later.
    val (errorBlock, errorArgs, errorModule) = createErrorBlock(newName _,
                                                                "tungsten.CastException",
                                                                "tungsten.CastException.ctor",
                                                                Nil,
                                                                block, module)

    // Create the successful case code. If the check succeeds, we just bitcast the 
    // old value to the new type.
    val castInst = BitCastInstruction(checkedcastInst.name, 
                                      checkedcastInst.ty,
                                      checkedcastInst.value)

    // Split the block. We need to add the checking code in a separate block which 
    // can call the error block.
    SplitBlock(List(castInst), List(errorBlock), errorModule,
               { (splitBlockName, splitBlockArguments, splitModule) =>
      // Check the type using instanceof
      val instanceOfInst = InstanceOfInstruction(newName, BooleanType,
                                                 checkedcastInst.value, checkedcastInst.ty)
      val loweredInstanceOfInst = 
        convertInstanceOfInstruction(instanceOfInst, splitModule).rewritten
      
      // Branch to the split block or error block depending on result
      val condInst = ConditionalBranchInstruction(newName,
                                                  UnitType,
                                                  instanceOfInst.makeValue,
                                                  errorBlock.name,
                                                  errorArgs,
                                                  splitBlockName,
                                                  splitBlockArguments)
      RewrittenInstructions(loweredInstanceOfInst :+ condInst)
    })
  }

  def convertInstanceOfInstruction(instanceofInst: InstanceOfInstruction,
                                   module: Module): RewrittenInstructions =
  {
    val valueInst = BitCastInstruction(symbolFactory(instanceofInst.name),
                                       PointerType(StructType(LowerPass.classStructName(module.rootClass.name))),
                                       instanceofInst.value)
    val value = valueInst.makeValue

    val isaType = instanceofInst.isa.asInstanceOf[ObjectDefinitionType]
    val isaValue = if (isaType.isInstanceOf[ClassType]) {
      DefinedValue(LowerPass.definitionInfoGlobalName(isaType.definitionName),
                   PointerType(StructType(LowerPass.classInfoStructName)))
    } else {
      val rawValue = DefinedValue(LowerPass.definitionInfoGlobalName(isaType.definitionName),
                                  PointerType(StructType(LowerPass.interfaceInfoStructName)))
      BitCastValue(rawValue, PointerType(StructType(LowerPass.classInfoStructName)))
    }

    val typeArgsType = PointerType(PointerType(StructType(LowerPass.classInfoStructName)),
                                   ReferenceType.NULLABLE)
    val typeArgs = isaType.typeArguments
    val (typeArgsInsts, typeArgsValue) = if (typeArgs.isEmpty) {
      (Nil, BitCastValue(NullValue, typeArgsType))
    } else {
      val wordCount = countTypeArgumentWords(typeArgs)
      val allocInst = StackAllocateArrayInstruction(symbolFactory(instanceofInst.name),
                                                    PointerType(PointerType(StructType(LowerPass.classInfoStructName))),
                                                    IntValue.word(wordCount, module))
      val storeInsts = storeTypeArgumentWords(allocInst.makeValue,
                                              0,
                                              typeArgs,
                                              { () => symbolFactory(instanceofInst.name) },
                                              module)

      val castValue = BitCastValue(allocInst.makeValue, typeArgsType)
      (allocInst :: storeInsts, castValue)
    }

    val callInst = StaticCallInstruction(instanceofInst.name,
                                         BooleanType,
                                         "tungsten.instanceof",
                                         Nil,
                                         List(value, isaValue, typeArgsValue))
    RewrittenInstructions(List(valueInst) ++ typeArgsInsts ++ List(callInst))
  }

  def convertNewInstruction(newInst: NewInstruction, module: Module): RewrittenInstructions = {
    val classType = newInst.ty.asInstanceOf[ClassType]
    val className = classType.definitionName
    val clas = module.getClass(className)

    // Every object has extra words at the end corresponding to its type arguments.
    // The number of words corresponds to the number of type arguments, including
    // sub-arguments. For example if we are constructing A[B[C, D], E[F, G]], then
    // we would need 6 additional words for: B, C, D, E, F, G. Objects with no type
    // arguments require no extra data.

    val wordSize = IntType.wordSize(module)/8
    val typeArgWords = countTypeArgumentWords(classType.typeArguments)
    val objectSize = clas.size(module)
    val totalSize = align(objectSize, wordSize) + typeArgWords * wordSize

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
    val typeArgumentOffset = align(objectSize, wordSize)/wordSize
    val storeInsts = storeTypeArgumentWords(classInfoInst.makeValue,
                                            typeArgumentOffset,
                                            classType.typeArguments,
                                            { () => symbolFactory(newInst.name + "new$") },
                                            module)
    
    // Now the object is initialized, and we can actually call the constructor
    val initInst = StaticCallInstruction(symbolFactory(newInst.name + "new$"),
                                         UnitType,
                                         newInst.constructorName,
                                         Nil,
                                         vtBitcastInst.makeValue :: newInst.arguments)
    val rewritten = List(allocInst, vtBitcastInst, vtableInst, classInfoInst) ++
      storeInsts ++ List(initInst)
    RewrittenInstructions(rewritten)
  }

  def convertNullCheckInstruction(nullcheckInst: NullCheckInstruction,
                                  block: Block,
                                  module: Module): SplitBlock =
  {
    def newName = symbolFactory(nullcheckInst.name + "nullcheck$")

    // Create a new error handling block. We create a new one for every nullcheck
    // since different nullchecks may have different exception handlers. Hopefully,
    // we can optimize out redundant ones later.
    val (errorBlock, errorArgs, errorModule) = 
      createErrorBlock(newName _,
                       "tungsten.NullPointerException",
                       "tungsten.NullPointerException.ctor",
                       Nil, block, module)

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
      val checkInst = RelationalOperatorInstruction(newName,
                                                    BooleanType,
                                                    RelationalOperator.EQUAL,
                                                    nullcheckInst.value,
                                                    nullValue)
      val branchInst = ConditionalBranchInstruction(newName,
                                                    UnitType,
                                                    checkInst.makeValue,
                                                    errorBlock.name,
                                                    errorArgs,
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
        val infoValue = DefinedValue(definitionInfoGlobalName(interfaceType.definitionName),
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
        val infoValue = DefinedValue(definitionInfoGlobalName(interfaceType.definitionName),
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
