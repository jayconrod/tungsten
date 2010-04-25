package tungsten

import java.io.File
import Utilities._

sealed abstract class AstDefinition {
  def compileDeclaration(ctx: AstContext): Unit
  def compile(ctx: AstContext): Definition
}

// Types
sealed abstract class AstType {
  def compile(ctx: AstContext): Type
  final def compileOrElse(ctx: AstContext, default: Type = UnitType) = {
    try {
      compile(ctx)
    } catch {
      case exn: CompileException => {
        ctx.errors += exn
        default
      }
    }
  }
}

final case object AstUnitType 
  extends AstType
{
  def compile(ctx: AstContext) = UnitType
}

final case object AstBooleanType
  extends AstType 
{
  def compile(ctx: AstContext) = BooleanType
}

final case class AstIntType(val width: Int)
  extends AstType
{
  if (width < 8 || width > 64 || !isPowerOf2(width))
    throw new IllegalArgumentException
  def compile(ctx: AstContext) = IntType(width)
}

final case class AstFloatType(val width: Int)
  extends AstType
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException
  def compile(ctx: AstContext) = FloatType(width)
}

final case class AstPointerType(elementType: AstType)
  extends AstType
{
  def compile(ctx: AstContext) = PointerType(elementType.compile(ctx))
}

final case object AstNullType
  extends AstType
{
  def compile(ctx: AstContext) = NullType
}

final case class AstArrayType(size: Option[Int],
                              elementType: AstType)
  extends AstType
{
  def compile(ctx: AstContext) = ArrayType(size, elementType.compile(ctx))
}

final case class AstClassType(val name: Symbol,
                              val typeArguments: List[AstType])
  extends AstType
{
  def compile(ctx: AstContext) = {
    val cTypeArguments = typeArguments.map(_.compile(ctx))
    ctx.module.getDefn(name) match {
      case Some(s: Struct) => StructType(s.name)
      case Some(other) => {
        ctx.errors += InappropriateSymbolException(name, Nowhere, "type")
        UnitType
      }
      case None => {
        ctx.errors += UndefinedSymbolException(name, Nowhere)
        UnitType
      }
    }
  }
}

// Values
sealed abstract class AstValue {
  def compile(ctx: AstContext): Value
  final def compileOrElse(ctx: AstContext, default: Value = UnitValue) = {
    try {
      compile(ctx)
    } catch {
      case exn: CompileException => {
        ctx.errors += exn
        default
      }
    }
  }
}

final case object AstUnitValue
  extends AstValue
{
  def compile(ctx: AstContext) = UnitValue
}

final case class AstBooleanValue(value: Boolean)
  extends AstValue
{
  def compile(ctx: AstContext) = BooleanValue(value)
}

final case class AstInt8Value(value: Byte)
  extends AstValue
{
  def compile(ctx: AstContext) = Int8Value(value)
}

final case class AstInt16Value(value: Short)
  extends AstValue
{
  def compile(ctx: AstContext) = Int16Value(value)
}

final case class AstInt32Value(value: Int)
  extends AstValue
{
  def compile(ctx: AstContext) = Int32Value(value)
}

final case class AstInt64Value(value: Long)
  extends AstValue
{
  def compile(ctx: AstContext) = Int64Value(value)
}

final case class AstFloat32Value(value: Float)
  extends AstValue
{
  def compile(ctx: AstContext) = Float32Value(value)
}

final case class AstFloat64Value(value: Double)
  extends AstValue
{
  def compile(ctx: AstContext) = Float64Value(value)
}

final case object AstNullValue
  extends AstValue
{
  def compile(ctx: AstContext) = NullValue
}

final case class AstArrayValue(elementType: AstType, 
                               elements: List[AstValue])
  extends AstValue
{
  def compile(ctx: AstContext) = {
    val cElementType = elementType.compile(ctx)
    val cElements = elements.map(_.compile(ctx))
    ArrayValue(cElementType, cElements)
  }
}

final case class AstAggregateValue(aggregateName: Symbol,
                                   fields: List[AstValue])
  extends AstValue
{
  def compile(ctx: AstContext) = {
    val cFields = fields.map(_.compile(ctx))
    ctx.module.getDefn(aggregateName) match {
      case Some(s: Struct) => StructValue(aggregateName, cFields)
      case Some(other) => {
        ctx.errors += InappropriateSymbolException(aggregateName, 
                                                   Nowhere,
                                                   "aggregate name")
        UnitValue
      }
      case None => {
        ctx.errors += UndefinedSymbolException(aggregateName, Nowhere)
        UnitValue
      }
    }
  }
}

final case class AstSymbolValue(value: Symbol)
  extends AstValue
{
  def compile(ctx: AstContext) = {
    ctx.resolve(value) match {
      case Some(name) => DefinedValue(name)
      case None => DefinedValue(value)
    }
  }
}

// Instructions

sealed abstract class AstInstruction(val name: Symbol)
  extends AstDefinition
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cInst = AssignInstruction(fullName, UnitValue, Nil)
    ctx.addDefn(cInst)
  }

  def compile(ctx: AstContext): Instruction
}

final case class AstAddressInstruction(override name: Symbol,
                                       base: AstValue,
                                       indices: List[AstValue])
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cBase = base.compile(ctx)
    val cIndices = indices.map(_.compile(ctx))
    val cAddress = AddressInstruction(fullName, cBase, cIndices, Nil)
    ctx.replaceDefn(cAddress)
    cAddress
  }
}

final case class AstAssignInstruction(override name: Symbol,
                                      target: AstValue)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cTarget = target.compile(ctx)
    val cInst = AssignInstruction(fullName, cTarget, Nil)
    ctx.replaceDefn(cInst)
    cInst
  }
}

final case class AstBinaryOperatorInstruction(override name: Symbol,
                                              operator: BinaryOperator,
                                              left: AstValue,
                                              right: AstValue)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cLeft = left.compile(ctx)
    val cRight = right.compile(ctx)
    val cBinop = BinaryOperatorInstruction(fullName, operator, cLeft, cRight, Nil)
    ctx.replaceDefn(cBinop)
    cBinop
  }
}

final case class AstBranchInstruction(override name: Symbol,
                                      target: Symbol,
                                      arguments: List[AstValue])
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cTarget = ctx.resolve(target) match {
      case Some(t) => t
      case None => target
    }
    val cArgs = arguments.map(_.compileOrElse(ctx))
    val cInst = BranchInstruction(fullName, cTarget, cArgs, Nil)
    ctx.replaceDefn(cInst)
    cInst
  }
}

final case class AstConditionalBranchInstruction(override name: Symbol,
                                                 condition: AstValue,
                                                 trueTarget: Symbol,
                                                 trueArguments: List[AstValue],
                                                 falseTarget: Symbol,
                                                 falseArguments: List[AstValue])
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    def resolveTarget(target: Symbol) = {
      ctx.resolve(target) match {
        case Some(resolved) => resolved
        case None => target
      }
    }

    val fullName = ctx.createName(name)
    val cCond = condition.compile(ctx)
    val cTrueTarget = resolveTarget(trueTarget)
    val cTrueArguments = trueArguments.map(_.compile(ctx))
    val cFalseTarget = resolveTarget(falseTarget)
    val cFalseArguments = falseArguments.map(_.compile(ctx))
    val cInst = ConditionalBranchInstruction(fullName, cCond, 
                                             cTrueTarget, cTrueArguments,
                                             cFalseTarget, cFalseArguments,
                                             Nil)
    ctx.replaceDefn(cInst)
    cInst
  }
}

final case class AstFloatExtendInstruction(override name: Symbol,
                                           value: AstValue,
                                           ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = FloatExtendInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

final case class AstFloatToIntegerInstruction(override name: Symbol,
                                              value: AstValue,
                                              ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = FloatToIntegerInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

final case class AstFloatTruncateInstruction(override name: Symbol,
                                             value: AstValue,
                                             ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = FloatTruncateInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

final case class AstHeapAllocateInstruction(override name: Symbol,
                                            ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cTy = ty.compile(ctx)
    val cAlloc = HeapAllocateInstruction(fullName, cTy, Nil)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstHeapAllocateArrayInstruction(override name: Symbol, 
                                                 count: AstValue,
                                                 elementType: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cCount = count.compile(ctx)
    val cElementType = elementType.compile(ctx)
    val cAlloc = HeapAllocateArrayInstruction(fullName, cCount, cElementType, Nil)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstIntegerSignExtendInstruction(override name: Symbol,
                                                 value: AstValue,
                                                 ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = IntegerSignExtendInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

final case class AstIntegerToFloatInstruction(override name: Symbol,
                                                 value: AstValue,
                                                 ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = IntegerToFloatInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

final case class AstIntegerTruncateInstruction(override name: Symbol,
                                               value: AstValue,
                                               ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = IntegerTruncateInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

final case class AstIntegerZeroExtendInstruction(override name: Symbol,
                                                 value: AstValue,
                                                 ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = IntegerZeroExtendInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

final case class AstIntrinsicCallInstruction(override name: Symbol,
                                             target: Symbol,
                                             arguments: List[AstValue])
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    import Intrinsic._
    val fullName = ctx.createName(name)
    val cIntrinsic = target.toString match {
      case "exit" => EXIT
    }
    val cArgs = arguments.map(_.compile(ctx))
    val cCall = IntrinsicCallInstruction(fullName, cIntrinsic, cArgs, Nil)
    ctx.replaceDefn(cCall)
    cCall
  }
}

final case class AstLoadInstruction(override name: Symbol,
                                    pointer: AstValue)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cPointer = pointer.compileOrElse(ctx)
    val cLoad = LoadInstruction(fullName, cPointer, Nil)
    ctx.replaceDefn(cLoad)
    cLoad
  }
}

final case class AstLoadElementInstruction(override name: Symbol,
                                           base: AstValue,
                                           indices: List[AstValue])
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cBase = base.compileOrElse(ctx)
    val cIndices = indices.map(_.compileOrElse(ctx))
    val cLoad = LoadElementInstruction(fullName, cBase, cIndices, Nil)
    ctx.replaceDefn(cLoad)
    cLoad
  }
}

final case class AstRelationalOperatorInstruction(override name: Symbol,
                                                  operator: RelationalOperator,
                                                  left: AstValue,
                                                  right: AstValue)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cLeft = left.compile(ctx)
    val cRight = right.compile(ctx)
    val cRelop = RelationalOperatorInstruction(fullName, operator, cLeft, cRight, Nil)
    ctx.replaceDefn(cRelop)
    cRelop
  }
}

final case class AstReturnInstruction(override name: Symbol,
                                      value: AstValue)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compileOrElse(ctx)
    val cReturn = ReturnInstruction(fullName, cValue, Nil)
    ctx.replaceDefn(cReturn)
    cReturn
  }
}

final case class AstStackAllocateInstruction(override name: Symbol,
                                             ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cTy = ty.compile(ctx)
    val cAlloc = StackAllocateInstruction(fullName, cTy, Nil)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstStackAllocateArrayInstruction(override name: Symbol,
                                                  count: AstValue,
                                                  elementType: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cCount = count.compile(ctx)
    val cElementType = elementType.compile(ctx)
    val cAlloc = StackAllocateArrayInstruction(fullName, cCount, cElementType, Nil)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstStaticCallInstruction(override name: Symbol,
                                          target: Symbol,
                                          arguments: List[AstValue])
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cArgs = arguments.map(_.compile(ctx))
    val cCall = StaticCallInstruction(fullName, target, cArgs, Nil)
    ctx.replaceDefn(cCall)
    cCall
  }
}

final case class AstStoreInstruction(override name: Symbol,
                                     pointer: AstValue,
                                     value: AstValue)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cPointer = pointer.compileOrElse(ctx)
    val cValue = value.compileOrElse(ctx)
    val cStore = StoreInstruction(fullName, cPointer, cValue, Nil)
    ctx.replaceDefn(cStore)
    cStore
  }
}

final case class AstStoreElementInstruction(override name: Symbol,
                                            base: AstValue,
                                            indices: List[AstValue],
                                            value: AstValue)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cBase = base.compileOrElse(ctx)
    val cIndices = indices.map(_.compileOrElse(ctx))
    val cValue = value.compileOrElse(ctx)
    val cStore = StoreElementInstruction(fullName, cBase, cIndices, cValue, Nil)
    ctx.replaceDefn(cStore)
    cStore
  }
}   

final case class AstUpcastInstruction(override name: Symbol,
                                      value: AstValue,
                                      ty: AstType)
  extends AstInstruction(name)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = UpcastInstruction(fullName, cValue, cTy, Nil)
    ctx.replaceDefn(cCast)
    cCast
  }
}

// Function and parameters

final case class AstParameter(name: Symbol, ty: AstType)
  extends AstDefinition
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cParam = Parameter(fullName, UnitType, Nil)
    ctx.addDefn(cParam)
  }

  def compile(ctx: AstContext): Parameter = {
    val cty = ty.compileOrElse(ctx)
    val fullName = ctx.createName(name)
    val cParam = Parameter(fullName, cty, Nil)
    ctx.replaceDefn(cParam)
    cParam
  }
}     

final case class AstBlock(name: Symbol,
                          parameters: List[AstParameter],
                          instructions: List[AstInstruction])
  extends AstDefinition
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    ctx.names.push(fullName)
    parameters.foreach(_.compileDeclaration(ctx))
    instructions.foreach(_.compileDeclaration(ctx))
    val cBlock = Block(fullName, Nil, Nil, Nil)
    ctx.names.pop
    ctx.addDefn(cBlock)
  }

  def compile(ctx: AstContext): Block = {
    val fullName = ctx.createName(name)
    ctx.names.push(fullName)
    val cParams = parameters.map(_.compile(ctx).name)
    val cInsts = instructions.map(_.compile(ctx).name)
    val cBlock = Block(fullName, cParams, cInsts, Nil)
    ctx.replaceDefn(cBlock)
    ctx.names.pop
    cBlock
  }
}

final case class AstFunction(name: Symbol,
                             returnType: AstType,
                             parameters: List[AstParameter],
                             blocks: List[AstBlock])
  extends AstDefinition
{
  def compileDeclaration(ctx: AstContext) = {
    ctx.names.push(name)
    parameters.foreach(_.compileDeclaration(ctx))
    blocks.foreach(_.compileDeclaration(ctx))
    val cFunction = Function(name, Nil, UnitType, Nil, Nil)
    ctx.names.pop
    ctx.addDefn(cFunction)
  }

  def compile(ctx: AstContext) = {
    ctx.names.push(name)
    val cRetTy = returnType.compileOrElse(ctx)
    val cParams = parameters.map(_.compile(ctx).name)
    var cBlocks = blocks.map(_.compile(ctx).name)
    val cFunction = Function(name, cParams, cRetTy, cBlocks, Nil)
    ctx.replaceDefn(cFunction)
    ctx.names.pop
    cFunction
  }
}

// Global

final case class AstGlobal(name: Symbol, 
                           ty: AstType, 
                           value: Option[AstValue])
  extends AstDefinition
{
  def compileDeclaration(ctx: AstContext) = {
    val cGlobal = Global(name, UnitType, None, Nil)
    ctx.addDefn(cGlobal)
  }

  def compile(ctx: AstContext) = {
    val global = Global(name, ty.compile(ctx), value.map(_.compile(ctx)), Nil)
    ctx.replaceDefn(global)
    global
  }
}

// Data structures

final case class AstField(name: Symbol,
                          ty: AstType)
  extends AstDefinition
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cField = Field(fullName, UnitType, Nil)
    ctx.addDefn(cField)
  }

  def compile(ctx: AstContext) = {
    val fullName = ctx.createName(name)
    val cField = Field(fullName, ty.compile(ctx), Nil)
    ctx.replaceDefn(cField)
    cField
  }
}

final case class AstStruct(name: Symbol,
                           fields: List[AstField])
  extends AstDefinition
{
  def compileDeclaration(ctx: AstContext) = {
    ctx.names.push(name)
    fields.foreach(_.compileDeclaration(ctx))
    val cStruct = Struct(name, Nil, Nil)
    ctx.names.pop
    ctx.addDefn(cStruct)
  }

  def compile(ctx: AstContext) = {
    ctx.names.push(name)
    val cFields = fields.map(_.compile(ctx).name)
    val cStruct = Struct(name, cFields, Nil)
    ctx.replaceDefn(cStruct)
    ctx.names.pop
    cStruct
  }
}

// Module

final class AstModule(val name: Symbol,
                      val ty: ModuleType,
                      val version: Version,
                      val filename: Option[File],
                      val dependencies: List[ModuleDependency],
                      val searchPaths: List[File],
                      val is64Bit: Boolean,
                      val isSafe: Boolean,
                      val definitions: List[AstDefinition])
{
  def this(definitions: List[AstDefinition]) = {
    this("default",
         ModuleType.INTERMEDIATE,
         Version.MIN,
         None,
         Nil,
         Nil,
         Utilities.isJvm64Bit,
         false,
         definitions)
  }

  def compile: Either[Module, List[CompileException]] = {
    val ctx = new AstContext(name, ty, version, filename, dependencies, searchPaths, is64Bit, isSafe)
    definitions.foreach(_.compileDeclaration(ctx))
    definitions.foreach(_.compile(ctx))
    ctx.errors.toList match {
      case Nil => Left(ctx.module)
      case errors => Right(errors)
    }
  }

  override def equals(that: Any) = {
    that match {
      case m: AstModule => {
        name         == m.name         &&
        ty           == m.ty           &&
        version      == m.version      &&
        filename     == m.filename     &&
        dependencies == m.dependencies &&
        searchPaths  == m.searchPaths  &&
        is64Bit      == m.is64Bit      &&
        isSafe       == m.isSafe       &&
        definitions  == m.definitions
      }
      case _ => false
    }
  }

  override def hashCode = hash(name, ty, version, filename, dependencies, searchPaths, is64Bit, isSafe, definitions)

  override def toString = {
    "AstModule(%s, %s, %s, %s, %s, %s, %s)".
      format(name, ty, version, filename, dependencies, searchPaths, is64Bit, definitions)
  }
}
