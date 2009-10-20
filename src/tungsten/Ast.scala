package tungsten

import Utilities._

sealed abstract class AstDefinition(val location: Location) {
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

final case class AstIntType(val width: Int, override val location: Location)
  extends AstType(location)
{
  if (width < 8 || width > 64 || !isPowerOf2(width))
    throw new IllegalArgumentException
  def compile(ctx: AstContext) = IntType(width, location)
}

final case class AstClassType(val name: Symbol,
                              val typeArguments: List[AstType],
                              override val location: Location)
  extends AstType(location)
{
  def compile(ctx: AstContext) = {
    ctx.module.get(name) match {
      case Some(c: Class) => ClassType(c.name, typeArguments.map(_.compile(ctx)), location)
      case Some(i: Interface) => {
        InterfaceType(i.name, typeArguments.map(_.compile(ctx)), location)
      }
      case Some(other) => {
        throw InappropriateSymbolException(name, location, other.location, "type")
      }
      case None => throw UndefinedSymbolException(name, location)
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
  def compile(ctx: AstContext): Instruction
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
      case None => throw new UndefinedSymbolException(target, location)
    }
    val cArgs = arguments.map(_.compileOrElse(ctx))
    val cInst = BranchInstruction(fullName, cTarget, cArgs, location)
    ctx.module.add(cInst)
    cInst
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
    ctx.module.add(cReturn)
    cReturn
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
    ctx.module.add(cCall)
    cCall
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
    ctx.module.add(cCall)
    cCall
  }
}

// Function and parameters

final case class AstParameter(name: Symbol, ty: AstType, override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext): Parameter = {
    val cty = ty.compileOrElse(ctx)
    val fullName = ctx.names.top + name
    val cparam = Parameter(fullName, cty, location)
    ctx.module.add(cparam)
    cparam
  }
}     

final case class AstTypeParameter(name: Symbol, 
                                  upperBound: Option[AstType], 
                                  lowerBound: Option[AstType],  
                                  override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext): TypeParameter = {
    val fullName = ctx.names.top + name
    val cUpperBound = upperBound.map(_.compileOrElse(ctx))
    val cLowerBound = lowerBound.map(_.compileOrElse(ctx))
    val cTyParam = TypeParameter(fullName, cUpperBound, cLowerBound, location)
    ctx.module.add(cTyParam)
    cTyParam
  }
}

final case class AstBlock(name: Symbol,
                          parameters: List[AstParameter],
                          instructions: List[AstInstruction],
                          override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext): Block = {
    val fullName = ctx.names.top + name
    val cParams = parameters.map(_.compile(ctx).name)
    val cInsts = instructions.map(_.compile(ctx).name)
    val cBlock = Block(fullName, cParams, cInsts, location)
    ctx.module.add(cBlock)
    cBlock
  }
}

final case class AstFunction(name: Symbol,
                             returnType: AstType,
                             typeParameters: List[AstTypeParameter],
                             parameters: List[AstParameter],
                             blocks: List[AstBlock],
                             override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = {
    ctx.names.push(name)
    val cRetTy = returnType.compileOrElse(ctx)
    val cTyParams = typeParameters.map(_.compile(ctx).name)
    val cParams = parameters.map(_.compile(ctx).name)
    val cBlocks = blocks.map(_.compile(ctx).name)
    val cFunction = Function(name, cTyParams, cParams, cRetTy, cBlocks, location)
    ctx.module.add(cFunction)
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
  def compile(ctx: AstContext) = {
    val global = Global(name, ty.compile(ctx), value.map(_.compile(ctx)), location)
    ctx.module.add(global)
    global
  }
}

// Data structures

final case class AstField(name: Symbol,
                          ty: AstType,
                          override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

final case class AstStruct(name: Symbol,
                           typeParameters: List[AstTypeParameter],
                           fields: List[AstField],
                           override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

final case class AstClass(name: Symbol,
                          typeParameters: List[AstTypeParameter],
                          superclass: Option[AstType],
                          interfaces: List[AstType],
                          fields: List[AstField],
                          methods: List[AstFunction],
                          override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

final case class AstInterface(name: Symbol,
                              typeParameters: List[AstTypeParameter],
                              superclass: Option[AstType],
                              interfaces: List[AstType],
                              methods: List[AstFunction],
                              override location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

// Module

final case class AstModule(definitions: List[AstDefinition]) {
  def compile = {
    val ctx = new AstContext
    for (defn <- definitions) defn.compile(ctx)
    ctx.module
  }
}
