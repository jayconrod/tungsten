package tungsten

import Utilities._

sealed abstract class AstDefinition(val location: Location) {
  def compileDeclaration(ctx: AstContext): Unit
  def compile(ctx: AstContext): Definition
}

// Types
sealed abstract class AstType(val location: Location) {
  def compile(ctx: AstContext): Type
  final def compileOrElse(ctx: AstContext, default: Type = UnitType(location)) = {
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

final case class AstUnitType(override val location: Location) extends AstType(location) {
  def compile(ctx: AstContext) = UnitType(location)
}

final case class AstBooleanType(override val location: Location) extends AstType(location) {
  def compile(ctx: AstContext) = BooleanType(location)
}

final case class AstIntType(val width: Int, override val location: Location)
  extends AstType(location)
{
  if (width < 8 || width > 64 || !isPowerOf2(width))
    throw new IllegalArgumentException
  def compile(ctx: AstContext) = IntType(width, location)
}

final case class AstFloatType(val width: Int, override val location: Location)
  extends AstType(location)
{
  if (width != 32 && width != 64)
    throw new IllegalArgumentException
  def compile(ctx: AstContext) = FloatType(width, location)
}

final case class AstPointerType(elementType: AstType, override location: Location)
  extends AstType(location)
{
  def compile(ctx: AstContext) = PointerType(elementType.compile(ctx), location)
}

final case class AstNullType(override location: Location)
  extends AstType(location)
{
  def compile(ctx: AstContext) = NullType(location)
}

final case class AstArrayType(size: Option[Int],
                              elementType: AstType,
                              override val location: Location)
  extends AstType(location)
{
  def compile(ctx: AstContext) = ArrayType(size, elementType.compile(ctx), location)
}

final case class AstClassType(val name: Symbol,
                              val typeArguments: List[AstType],
                              override val location: Location)
  extends AstType(location)
{
  def compile(ctx: AstContext) = {
    val cTypeArguments = typeArguments.map(_.compile(ctx))
    ctx.module.getDefn(name) match {
      case Some(s: Struct) => StructType(s.name, location)
      case Some(other) => {
        ctx.errors += InappropriateSymbolException(name, location, other.location, "type")
        UnitType(location)
      }
      case None => {
        ctx.errors += UndefinedSymbolException(name, location)
        UnitType(location)
      }
    }
  }
}

// Values
sealed abstract class AstValue(val location: Location) {
  def compile(ctx: AstContext): Value
  final def compileOrElse(ctx: AstContext, default: Value = UnitValue(location)) = {
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

final case class AstUnitValue(override val location: Location) extends AstValue(location) {
  def compile(ctx: AstContext) = UnitValue(location)
}

final case class AstBooleanValue(value: Boolean, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = BooleanValue(value, location)
}

final case class AstInt8Value(value: Byte, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = Int8Value(value, location)
}

final case class AstInt16Value(value: Short, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = Int16Value(value, location)
}

final case class AstInt32Value(value: Int, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = Int32Value(value, location)
}

final case class AstInt64Value(value: Long, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = Int64Value(value, location)
}

final case class AstFloat32Value(value: Float, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = Float32Value(value, location)
}

final case class AstFloat64Value(value: Double, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = Float64Value(value, location)
}

final case class AstNullValue(override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = NullValue(location)
}

final case class AstArrayValue(elementType: AstType, 
                               elements: List[AstValue],
                               override location: Location = Nowhere)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = {
    val cElementType = elementType.compile(ctx)
    val cElements = elements.map(_.compile(ctx))
    ArrayValue(cElementType, cElements, location)
  }
}

final case class AstAggregateValue(aggregateName: Symbol,
                                   fields: List[AstValue],
                                   override location: Location = Nowhere)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = {
    val cFields = fields.map(_.compile(ctx))
    ctx.module.getDefn(aggregateName) match {
      case Some(s: Struct) => StructValue(aggregateName, cFields, location)
      case Some(other) => {
        ctx.errors += InappropriateSymbolException(aggregateName, 
                                                   location,
                                                   other.location,
                                                   "aggregate name")
        UnitValue(location)
      }
      case None => {
        ctx.errors += UndefinedSymbolException(aggregateName, location)
        UnitValue(location)
      }
    }
  }
}

final case class AstSymbolValue(value: Symbol, override location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = {
    ctx.resolve(value) match {
      case Some(name) => DefinedValue(name, location)
      case None => DefinedValue(value, location)
    }
  }
}

// Instructions

sealed abstract class AstInstruction(val name: Symbol, override val location: Location) 
  extends AstDefinition(location)
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName: Symbol = ctx.names.top + name
    val cInst = AssignInstruction(fullName, UnitValue(), location)
    ctx.addDefn(cInst)
  }

  def compile(ctx: AstContext): Instruction
}

final case class AstAddressInstruction(override name: Symbol,
                                       base: AstValue,
                                       indices: List[AstValue],
                                       override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cBase = base.compile(ctx)
    val cIndices = indices.map(_.compile(ctx))
    val cAddress = AddressInstruction(fullName, cBase, cIndices, location)
    ctx.replaceDefn(cAddress)
    cAddress
  }
}

final case class AstAssignInstruction(override name: Symbol,
                                      target: AstValue,
                                      override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cTarget = target.compile(ctx)
    val cInst = AssignInstruction(fullName, cTarget, location)
    ctx.replaceDefn(cInst)
    cInst
  }
}

final case class AstBinaryOperatorInstruction(override name: Symbol,
                                              operator: BinaryOperator,
                                              left: AstValue,
                                              right: AstValue,
                                              override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cLeft = left.compile(ctx)
    val cRight = right.compile(ctx)
    val cBinop = BinaryOperatorInstruction(fullName, operator, cLeft, cRight, location)
    ctx.replaceDefn(cBinop)
    cBinop
  }
}

final case class AstBranchInstruction(override name: Symbol,
                                      target: Symbol,
                                      arguments: List[AstValue],
                                      override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cTarget = ctx.resolve(target) match {
      case Some(t) => t
      case None => target
    }
    val cArgs = arguments.map(_.compileOrElse(ctx))
    val cInst = BranchInstruction(fullName, cTarget, cArgs, location)
    ctx.replaceDefn(cInst)
    cInst
  }
}

final case class AstConditionalBranchInstruction(override name: Symbol,
                                                 condition: AstValue,
                                                 trueTarget: Symbol,
                                                 trueArguments: List[AstValue],
                                                 falseTarget: Symbol,
                                                 falseArguments: List[AstValue],
                                                 override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    def resolveTarget(target: Symbol) = {
      ctx.resolve(target) match {
        case Some(resolved) => resolved
        case None => target
      }
    }

    val fullName = ctx.names.top + name
    val cCond = condition.compile(ctx)
    val cTrueTarget = resolveTarget(trueTarget)
    val cTrueArguments = trueArguments.map(_.compile(ctx))
    val cFalseTarget = resolveTarget(falseTarget)
    val cFalseArguments = falseArguments.map(_.compile(ctx))
    val cInst = ConditionalBranchInstruction(fullName, cCond, 
                                             cTrueTarget, cTrueArguments,
                                             cFalseTarget, cFalseArguments,
                                             location)
    ctx.replaceDefn(cInst)
    cInst
  }
}

final case class AstHeapAllocateInstruction(override name: Symbol,
                                            ty: AstType,
                                            override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cTy = ty.compile(ctx)
    val cAlloc = HeapAllocateInstruction(fullName, cTy, location)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstHeapAllocateArrayInstruction(override name: Symbol, 
                                                 count: AstValue,
                                                 elementType: AstType,
                                                 override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cCount = count.compile(ctx)
    val cElementType = elementType.compile(ctx)
    val cAlloc = HeapAllocateArrayInstruction(fullName, cCount, cElementType, location)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstIndirectCallInstruction(override name: Symbol,
                                            target: AstValue,
                                            arguments: List[AstValue],
                                            override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cTarget = target.compileOrElse(ctx)
    val cArgs = arguments.map(_.compile(ctx))
    val cCall = IndirectCallInstruction(fullName, cTarget, cArgs, location)
    ctx.replaceDefn(cCall)
    cCall
  }
}

final case class AstIntrinsicCallInstruction(override name: Symbol,
                                             target: Symbol,
                                             arguments: List[AstValue],
                                             override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    import Intrinsic._
    val fullName = ctx.names.top + name
    val cIntrinsic = target.toString match {
      case "exit" => EXIT
    }
    val cArgs = arguments.map(_.compile(ctx))
    val cCall = IntrinsicCallInstruction(fullName, cIntrinsic, cArgs, location)
    ctx.replaceDefn(cCall)
    cCall
  }
}

final case class AstLoadInstruction(override name: Symbol,
                                    pointer: AstValue,
                                    override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cPointer = pointer.compileOrElse(ctx)
    val cLoad = LoadInstruction(fullName, cPointer, location)
    ctx.replaceDefn(cLoad)
    cLoad
  }
}

final case class AstLoadElementInstruction(override name: Symbol,
                                           base: AstValue,
                                           indices: List[AstValue],
                                           override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cBase = base.compileOrElse(ctx)
    val cIndices = indices.map(_.compileOrElse(ctx))
    val cLoad = LoadElementInstruction(fullName, cBase, cIndices, location)
    ctx.replaceDefn(cLoad)
    cLoad
  }
}

final case class AstRelationalOperatorInstruction(override name: Symbol,
                                                  operator: RelationalOperator,
                                                  left: AstValue,
                                                  right: AstValue,
                                                  override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cLeft = left.compile(ctx)
    val cRight = right.compile(ctx)
    val cRelop = RelationalOperatorInstruction(fullName, operator, cLeft, cRight, location)
    ctx.replaceDefn(cRelop)
    cRelop
  }
}

final case class AstReturnInstruction(override name: Symbol,
                                      value: AstValue,
                                      override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cValue = value.compileOrElse(ctx)
    val cReturn = ReturnInstruction(fullName, cValue, location)
    ctx.replaceDefn(cReturn)
    cReturn
  }
}

final case class AstStackAllocateInstruction(override name: Symbol,
                                             ty: AstType,
                                             override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cTy = ty.compile(ctx)
    val cAlloc = StackAllocateInstruction(fullName, cTy, location)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstStackAllocateArrayInstruction(override name: Symbol,
                                                  count: AstValue,
                                                  elementType: AstType,
                                                  override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cCount = count.compile(ctx)
    val cElementType = elementType.compile(ctx)
    val cAlloc = StackAllocateArrayInstruction(fullName, cCount, cElementType, location)
    ctx.replaceDefn(cAlloc)
    cAlloc
  }
}

final case class AstStaticCallInstruction(override name: Symbol,
                                          target: Symbol,
                                          arguments: List[AstValue],
                                          override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cArgs = arguments.map(_.compile(ctx))
    val cCall = StaticCallInstruction(fullName, target, cArgs, location)
    ctx.replaceDefn(cCall)
    cCall
  }
}

final case class AstStoreInstruction(override name: Symbol,
                                     pointer: AstValue,
                                     value: AstValue,
                                     override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cPointer = pointer.compileOrElse(ctx)
    val cValue = value.compileOrElse(ctx)
    val cStore = StoreInstruction(fullName, cPointer, cValue, location)
    ctx.replaceDefn(cStore)
    cStore
  }
}

final case class AstStoreElementInstruction(override name: Symbol,
                                            base: AstValue,
                                            indices: List[AstValue],
                                            value: AstValue,
                                            override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cBase = base.compileOrElse(ctx)
    val cIndices = indices.map(_.compileOrElse(ctx))
    val cValue = value.compileOrElse(ctx)
    val cStore = StoreElementInstruction(fullName, cBase, cIndices, cValue, location)
    ctx.replaceDefn(cStore)
    cStore
  }
}   

final case class AstUpcastInstruction(override name: Symbol,
                                      value: AstValue,
                                      ty: AstType,
                                      override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cValue = value.compile(ctx)
    val cTy = ty.compile(ctx)
    val cCast = UpcastInstruction(fullName, cValue, cTy, location)
    ctx.replaceDefn(cCast)
    cCast
  }
}

// Function and parameters

final case class AstParameter(name: Symbol, ty: AstType, override location: Location)
  extends AstDefinition(location)
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cParam = Parameter(fullName, UnitType(), location)
    ctx.addDefn(cParam)
  }

  def compile(ctx: AstContext): Parameter = {
    val cty = ty.compileOrElse(ctx)
    val fullName = ctx.names.top + name
    val cParam = Parameter(fullName, cty, location)
    ctx.replaceDefn(cParam)
    cParam
  }
}     

final case class AstBlock(name: Symbol,
                          parameters: List[AstParameter],
                          instructions: List[AstInstruction],
                          override location: Location)
  extends AstDefinition(location)
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    ctx.names.push(fullName)
    parameters.foreach(_.compileDeclaration(ctx))
    instructions.foreach(_.compileDeclaration(ctx))
    val cBlock = Block(fullName, Nil, Nil, location)
    ctx.names.pop
    ctx.addDefn(cBlock)
  }

  def compile(ctx: AstContext): Block = {
    val fullName = ctx.names.top + name
    ctx.names.push(fullName)
    val cParams = parameters.map(_.compile(ctx).name)
    val cInsts = instructions.map(_.compile(ctx).name)
    val cBlock = Block(fullName, cParams, cInsts, location)
    ctx.replaceDefn(cBlock)
    ctx.names.pop
    cBlock
  }
}

final case class AstFunction(name: Symbol,
                             returnType: AstType,
                             parameters: List[AstParameter],
                             blocks: List[AstBlock],
                             override location: Location)
  extends AstDefinition(location)
{
  def compileDeclaration(ctx: AstContext) = {
    ctx.names.push(name)
    parameters.foreach(_.compileDeclaration(ctx))
    blocks.foreach(_.compileDeclaration(ctx))
    val cFunction = Function(name, Nil, UnitType(), Nil, location)
    ctx.names.pop
    ctx.addDefn(cFunction)
  }

  def compile(ctx: AstContext) = {
    ctx.names.push(name)
    val cRetTy = returnType.compileOrElse(ctx)
    val cParams = parameters.map(_.compile(ctx).name)
    var cBlocks = blocks.map(_.compile(ctx).name)
    val cFunction = Function(name, cParams, cRetTy, cBlocks, location)
    ctx.replaceDefn(cFunction)
    ctx.names.pop
    cFunction
  }
}

// Global

final case class AstGlobal(name: Symbol, 
                           ty: AstType, 
                           value: Option[AstValue], 
                           override location: Location)
  extends AstDefinition(location)
{
  def compileDeclaration(ctx: AstContext) = {
    val cGlobal = Global(name, UnitType(), None, location)
    ctx.addDefn(cGlobal)
  }

  def compile(ctx: AstContext) = {
    val global = Global(name, ty.compile(ctx), value.map(_.compile(ctx)), location)
    ctx.replaceDefn(global)
    global
  }
}

// Data structures

final case class AstField(name: Symbol,
                          ty: AstType,
                          override location: Location)
  extends AstDefinition(location)
{
  def compileDeclaration(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cField = Field(fullName, UnitType(), location)
    ctx.addDefn(cField)
  }

  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cField = Field(fullName, ty.compile(ctx), location)
    ctx.replaceDefn(cField)
    cField
  }
}

final case class AstStruct(name: Symbol,
                           fields: List[AstField],
                           override location: Location)
  extends AstDefinition(location)
{
  def compileDeclaration(ctx: AstContext) = {
    ctx.names.push(name)
    fields.foreach(_.compileDeclaration(ctx))
    val cStruct = Struct(name, Nil, location)
    ctx.names.pop
    ctx.addDefn(cStruct)
  }

  def compile(ctx: AstContext) = {
    ctx.names.push(name)
    val cFields = fields.map(_.compile(ctx).name)
    val cStruct = Struct(name, cFields, location)
    ctx.replaceDefn(cStruct)
    ctx.names.pop
    cStruct
  }
}

// Module

final case class AstModule(definitions: List[AstDefinition]) {
  def compile: Either[Module, List[CompileException]] = {
    val ctx = new AstContext
    definitions.foreach(_.compileDeclaration(ctx))
    definitions.foreach(_.compile(ctx))
    ctx.errors.toList match {
      case Nil => Left(ctx.module)
      case errors => Right(errors)
    }
  }
}
