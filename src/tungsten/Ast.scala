package tungsten

sealed abstract class AstDefinition(val location: Location)


// Types

sealed abstract class AstType(val location: Location)

final case class AstUnitType(override val location: Location) extends AstType(location)

// Values

sealed abstract class AstValue(val location: Location)

final case class AstUnitValue(override val location: Location) extends AstValue(location)

// Instructions

sealed abstract class AstInstruction

// Function and parameters

final case class AstParameter(name: Symbol, ty: AstType)
final case class AstTypeParameter(name: Symbol, upperBound: AstType, lowerBound: AstType)


// Global

final case class AstGlobal(name: Symbol, 
                           ty: AstType, 
                           value: Option[AstValue], 
                           override val location: Location)
  extends AstDefinition(location)

// Data structures

// Module

final case class AstModule(definitions: List[AstDefinition])
