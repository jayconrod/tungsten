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

final case class EmptyComponentsException(symbol: Symbol,
                                          componentName: String,
                                          location: Location)
  extends CompileException(symbol.toString + " must contain at least one " + componentName, 
                           location)

final case class EntryParametersException(functionName: Symbol, 
                                          blockName: Symbol,
                                          location: Location)
  extends CompileException("the entry block " + blockName + " for function " + functionName +
                             " must not have any parameters",
                           location)

final case class FloatBitOperationException(location: Location)
  extends CompileException("bit operations are not supported on floating point values",
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

final case class GlobalValueNonLiteralException(symbol: Symbol, location: Location)
  extends CompileException("global " + symbol + " has a non-literal initial value", location)

final case class InappropriateSymbolException(symbol: Symbol,
                                              location: Location,
                                              defnLocation: Location,
                                              expected: String)
  extends CompileException(symbol.toString + " defined at " + defnLocation + 
                             " does not refer to a(n) " + expected,
                           location)

final case class InstructionOrderException(symbol: Symbol,
                                           location: Location)
  extends CompileException("the symbol " + symbol + 
                             " is not valid at the point where it is used",
                           location)

final case class InvalidIndexException(value: String, ty: String, location: Location)
  extends CompileException("The value " + value + 
                             " cannot be used as an index into type " + ty,
                           location)

final case class MainNonEmptyParametersException(location: Location)
  extends CompileException("main function must not have parameters", location)

final case class MainReturnTypeException(location: Location)
  extends CompileException("main function must have unit return type", location)

final case class MissingMainException()
  extends CompileException("module does not contain a main function", Nowhere)

final case class NonLocalBranchException(functionName: Symbol,
                                         blockName: Symbol,
                                         location: Location)
  extends CompileException("function " + functionName + 
                             " branches to non-local block " + blockName,
                           location)

final case class NumericTypeException(ty: String, location: Location)
  extends CompileException("type " + ty + " must be numeric", location)

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

final case class UpcastException(fromTy: String,
                                 toTy: String,
                                 location: Location)
  extends CompileException("value cannot be upcast from type " +
                             fromTy + " to type " + toTy,
                           location)
