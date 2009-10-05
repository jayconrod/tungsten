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

final case class AstSymbolValue(val value: Symbol, override val location: Location)
  extends AstValue(location)
{
  def compile(ctx: AstContext) = {
    ctx.resolve(value) match {
      case Some(defn) => DefinedValue(defn, location)
      case None => throw UndefinedSymbolException(value, location)
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
                                      target: AstValue,
                                      arguments: List[AstValue],
                                      override location: Location)
  extends AstInstruction(name, location)
{
  def compile(ctx: AstContext) = {
    val fullName = ctx.names.top + name
    val cTarget = target.compileOrElse(ctx)
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
    def fullName = ctx.names.top + name
    val cValue = value.compileOrElse(ctx)
    val cReturn = ReturnInstruction(fullName, cValue, location)
    ctx.module.add(cReturn)
    cReturn
  }
}

// Function and parameters

final case class AstParameter(val name: Symbol, val ty: AstType, override val location: Location)
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

final case class AstTypeParameter(val name: Symbol, 
                                  val upperBound: Option[AstType], 
                                  val lowerBound: Option[AstType],  
                                  override val location: Location)
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

final case class AstBlock(val name: Symbol,
                          val parameters: List[AstParameter],
                          val instructions: List[AstInstruction],
                          override val location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext): Block = {
    val fullName = ctx.names.top + name
    val cParams = parameters.map(_.compile(ctx))
    val cInsts = instructions.map(_.compile(ctx))
    val cBlock = Block(fullName, cParams, cInsts, location)
    ctx.module.add(cBlock)
    cBlock
  }
}

final case class AstFunction(val name: Symbol,
                             val returnType: AstType,
                             val typeParameters: List[AstTypeParameter],
                             val parameters: List[AstParameter],
                             val blocks: List[AstBlock],
                             override val location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

// Global

final case class AstGlobal(val name: Symbol, 
                           val ty: AstType, 
                           val value: Option[AstValue], 
                           override val location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = {
    val global = Global(name, ty.compile(ctx), value.map(_.compile(ctx)), location)
    ctx.module.add(global)
    global
  }
}

// Data structures

final case class AstField(val name: Symbol,
                          val ty: AstType,
                          override val location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

final case class AstStruct(val name: Symbol,
                           val typeParameters: List[AstTypeParameter],
                           val fields: List[AstField],
                           override val location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

final case class AstClass(val name: Symbol,
                          val typeParameters: List[AstTypeParameter],
                          val superclass: Option[AstType],
                          val interfaces: List[AstType],
                          val fields: List[AstField],
                          val methods: List[AstFunction],
                          override val location: Location)
  extends AstDefinition(location)
{
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

final case class AstInterface(val name: Symbol,
                              val typeParameters: List[AstTypeParameter],
                              val superclass: Option[AstType],
                              val interfaces: List[AstType],
                              val methods: List[AstFunction],
                              override val location: Location)
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
