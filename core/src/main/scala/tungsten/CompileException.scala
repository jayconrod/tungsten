package tungsten

/** A compile exception represents an error with a module. These are usually simply found and 
 *  returned as lists during validation. However, they are exceptions, so it also possible
 *  to throw them. All exceptions have a message (which will be printed for the user) and a 
 *  location that indicates where in the source the message came from. If the location is
 *  Nowhere, no location is printed.
 */
sealed abstract class CompileException(message: String, location: Location) extends Exception {
  override def toString = {
    val locStr = if (location == Nowhere) "" else location + ": "
    locStr + "error: " + message
  }
}

final case class AnnotationArgumentCountException(annotationName: Symbol,
                                                  given: Int,
                                                  required: Int,
                                                  location: Location)
  extends CompileException("annotation %s was given %d fields; %d are required".
                             format(annotationName, given, required),
                           location)

final case class ArrayTypeWidthException(length: Long, location: Location)
  extends CompileException("array type has length %d which is too large for a 32-bit module".
                             format(length),
                           location)

final case class BlockTerminationException(symbol: Symbol, location: Location)
  extends CompileException("block " + symbol + " does not terminate", location)

final case class CyclicStructException(structNames: List[Symbol], location: Location)
  extends CompileException("the following structs depend on each other: " + 
                             structNames.mkString(", "),
                           location)

final case class DuplicateComponentException(symbol: Symbol,
                                             component: Symbol,
                                             className: String,
                                             location: Location)
  extends CompileException("duplicate " + className + " " + component + 
                             " used as part of definition " + symbol,
                           location)

final case class DuplicateDependencyException(moduleName: Symbol,
                                              dependencyName: Symbol)
  extends CompileException("module " + moduleName + " lists " + dependencyName + 
                             " more than once as a dependency",
                           Nowhere)

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

final case class ExternalDefinitionException(name: Symbol)
  extends CompileException("symbol " + name + " is declared but never defined", Nowhere)

final case class FieldCountException(structName: Symbol,
                                     given: Int,
                                     required: Int,
                                     location: Location)
  extends CompileException("struct %s value has %d fields; %d are required".
                             format(structName, given, required),
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
                                              expected: String)
  extends CompileException(symbol.toString + " does not refer to a(n) " + expected,
                           location)

final case class InstructionOrderException(symbol: Symbol,
                                           location: Location)
  extends CompileException("the symbol " + symbol + 
                             " is not valid at the point where it is used",
                           location)

final case class IntegerRangeException(value: Long, width: Int, location: Location)
  extends CompileException("the integer %d cannot be stored in %d bits".format(value, width),
                           location)

final case class InvalidBitCastException(value: Value, 
                                         valueSize: Long, 
                                         ty: Type, 
                                         tySize: Long,
                                         location: Location)
  extends CompileException("value %s (%d bits) cannot be bit-cast to type %s (%d bits) because they have different sizes".
                             format(value, valueSize, ty, tySize),
                           location)

final case class InvalidIndexException(value: String, ty: String, location: Location)
  extends CompileException("the value " + value + 
                             " cannot be used as an index into type " + ty,
                           location)

final case class MainNonEmptyParametersException(location: Location)
  extends CompileException("main function must not have parameters", location)

final case class MainReturnTypeException(location: Location)
  extends CompileException("main function must have unit return type", location)

final case class MissingElementIndexException(location: Location)
  extends CompileException("no indices given for a pointer element instruction; at least one is required", location)

final case class MissingMainException()
  extends CompileException("module does not contain a main function", Nowhere)

final case class NonLocalBranchException(functionName: Symbol,
                                         blockName: Symbol,
                                         location: Location)
  extends CompileException("function " + functionName + 
                             " branches to non-local block " + blockName,
                           location)

final case class NumericExtensionException(fromTy: String, toTy: String, location: Location)
  extends CompileException("cannot extend from type " + fromTy + " to " + toTy, location)
                        

final case class NumericTruncationException(fromTy: String, toTy: String, location: Location)
  extends CompileException("cannot truncate from type " + fromTy + " to " + toTy, location)

final case class NumericTypeException(ty: String, location: Location)
  extends CompileException("type " + ty + " must be numeric", location)

final case class ParseException(message: String, location: Location)
  extends CompileException(message, location)

final case class RedefinedSymbolException(symbol: Symbol, 
                                          location: Location, 
                                          oldLocation: Location)
  extends CompileException(symbol.toString + " was redefined; original definition was at " + 
                             oldLocation,
                           location)

final case class TypeMismatchException(given: Any, required: Any, location: Location)
  extends CompileException("type mismatch: %s  was given; %s was required".
                             format(given, required),
                           location)

final case class UndefinedSymbolException(symbol: Symbol, location: Location)
  extends CompileException(symbol.toString + " is not defined", location)

final case class UnsupportedNumericOperationException(ty: AnyRef, op: AnyRef, location: Location)
  extends CompileException("the type %s does not support the operation %s".format(ty, op),
                           location)

final case class UpcastException(fromTy: String,
                                 toTy: String,
                                 location: Location)
  extends CompileException("value cannot be upcast from type " +
                             fromTy + " to type " + toTy,
                           location)
