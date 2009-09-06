package tungsten

sealed abstract class AstDefinition {
  var location: Location = Nowhere
}

// Types

sealed abstract class AstType {
  var location: Location = Nowhere
}

final case class AstUnitType() extends AstType

// Instructions

sealed abstract class AstInstruction

// Function and parameters

final case class AstParameter(name: Symbol, ty: AstType)
final case class AstTypeParameter(name: Symbol, upperBound: AstType, lowerBound: AstType)


// Global

final case class AstGlobal(name: Symbol, ty: AstType, value: Option[Value])
  extends AstDefinition

// Data structures

// Module

final case class AstModule(definitions: List[AstDefinition])
