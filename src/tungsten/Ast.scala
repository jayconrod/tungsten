package tungsten

sealed abstract class AstDefinition(location: Location)

// Types

sealed abstract class AstType(location: Location)

final case class AstUnitType(location: Location) extends AstType(location)

// Instructions

sealed abstract class AstInstruction(location: Location)

// Function and parameters

final case class AstParameter(name: Symbol, ty: AstType, location: Location)
final case class AstTypeParameter(name: Symbol, 
                                  upperBound: AstType,
                                  lowerBound: AstType,
                                  location: Location)

// Global

final case class AstGlobal(name: Symbol, ty: AstType, value: Option[Value], location: Location)
  extends AstDefinition(location)

// Data structures

// Module

final case class AstModule(definitions: List[AstDefinition])
