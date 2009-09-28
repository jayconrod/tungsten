package tungsten

import Utilities._

sealed abstract class AstDefinition(val location: Location) {
  def compile(ctx: AstContext): Unit
}

// Types
sealed abstract class AstType(val location: Location) {
  def compile(ctx: AstContext): Type
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
  def compile(ctx: AstContext) = throw new UnsupportedOperationException
}

// Instructions

sealed abstract class AstInstruction(val location: Location)

final case class AstBranchInstruction(val target: AstValue,
                                      val arguments: List[AstValue],
                                      override val location: Location)
  extends AstInstruction(location)
final case class AstReturnInstruction(val value: AstValue, override val location: Location)
  extends AstInstruction(location)

// Function and parameters

final case class AstParameter(val name: Symbol, val ty: AstType, val location: Location)
final case class AstTypeParameter(val name: Symbol, 
                                  val upperBound: Option[AstType], 
                                  val lowerBound: Option[AstType],  
                                  val location: Location)

final case class AstBlock(val name: Symbol,
                          val parameters: List[AstParameter],
                          val instructions: List[AstInstruction],
                          val location: Location)

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
    ctx.module.add(new Global(name, ty.compile(ctx), value.map(_.compile(ctx)), location))
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
