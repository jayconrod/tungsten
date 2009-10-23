package tungsten

sealed abstract class CompileException(message: String, location: Location) extends Exception {
  override def toString = {
    location + ": error: " + message
  }
}

final case class BlockTerminationException(symbol: Symbol, location: Location)
  extends CompileException("block " + symbol + " does not terminate", location)

final case class DuplicateComponentException(symbol: Symbol,
                                             component: Symbol,
                                             className: String,
                                             location: Location)
  extends CompileException("duplicate " + className + " " + component + 
                             " used as part of definition " + symbol,
                           location)

final case class EarlyTerminationException(blockName: Symbol,
                                           instName: Symbol,
                                           location: Location)
  extends CompileException("block " + instName + " terminates early on instruction " + 
                             instName,
                           location)

final case class EmptyBlockException(symbol: Symbol, location: Location)
  extends CompileException("block " + symbol + " contains no instructions", location)

final case class EmptyStructException(symbol: Symbol, location: Location)
  extends CompileException("struct " + symbol.toString + " must contain at least one field",
                           location)

final case class InappropriateSymbolException(symbol: Symbol,
                                              location: Location,
                                              defnLocation: Location,
                                              expected: String)
  extends CompileException(symbol.toString + " defined at " + defnLocation + 
                             " does not refer to a(n) " + expected,
                           location)

final case class FunctionArgumentCountException(symbol: Symbol,
                                                given: Int,
                                                required: Int,
                                                location: Location)
  extends CompileException(symbol.toString + " was called with " + given + 
                             " arguments; it expects " + required, 
                           location)

final case class FunctionTypeException(value: String, location: Location)
  extends CompileException("value cannot be called as a function", location)

final case class MissingMainException()
  extends CompileException("module does not contain a main function", Nowhere)

final case class RedefinedSymbolException(symbol: Symbol, 
                                          location: Location, 
                                          oldLocation: Location)
  extends CompileException(symbol.toString + " was redefined; original definition was at " + 
                             oldLocation,
                           location)

final case class TypeMismatchException(given: String, required: String, location: Location)
  extends CompileException("type mismatch: " + given + " was given; " +
                             required + " was required",
                           location)

final case class UndefinedSymbolException(symbol: Symbol, location: Location)
  extends CompileException(symbol.toString + " is not defined", location)
