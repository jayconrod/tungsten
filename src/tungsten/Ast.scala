package tungsten

import Utilities._

sealed abstract class AstDefinition(val location: Location)


// Types

sealed abstract class AstType(val location: Location)

final case class AstUnitType(override val location: Location) extends AstType(location)
final case class AstIntType(val width: Int, override val location: Location)
  extends AstType(location)
{
  if (width < 8 || width > 64 || !isPowerOf2(width))
    throw new IllegalArgumentException
}

// Values

sealed abstract class AstValue(val location: Location)

final case class AstUnitValue(override val location: Location) extends AstValue(location)
final case class AstIntValue(val value: Long, override val location: Location)
  extends AstValue(location)
final case class AstSymbolValue(val value: Symbol, override val location: Location)
  extends AstValue(location)

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

// Global

final case class AstGlobal(val name: Symbol, 
                           val ty: AstType, 
                           val value: Option[AstValue], 
                           override val location: Location)
  extends AstDefinition(location)

// Data structures

final case class AstField(val name: Symbol,
                          val ty: AstType,
                          override val location: Location)
  extends AstDefinition(location)

final case class AstStruct(val name: Symbol,
                           val typeParameters: List[AstTypeParameter],
                           val fields: List[AstField],
                           override val location: Location)
  extends AstDefinition(location)

// Module

final case class AstModule(definitions: List[AstDefinition])
