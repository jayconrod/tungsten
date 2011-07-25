/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class FunctionValidationTest
  extends ValidationTest
{
  override def programContainsError[T <: CompileException](program: String)(implicit m: Manifest[T]) {
    val module = compileString(program)
    val moduleWithRuntime = linkRuntime(module)
    val errors = moduleWithRuntime.validate
    containsError[T](errors)(m)
  }

  @Test
  def emptyBlockTermination {
    val program = "function unit @main( ) { block %empty }"
    programContainsError[EmptyComponentsException](program)
  }

  @Test
  def blockTermination {
    val program = "function unit @main { block %entry { unit %foo = scall @main() } }"
    programContainsError[BlockTerminationException](program)
  }

  @Test
  def earlyTermination {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[EarlyTerminationException](program)
  }

  @Test
  def exitTermination {
    val program = "function unit @main { block %entry { intrinsic exit(int32 12) } }"
    programIsCorrect(program)
  }

  @Test
  def returnTypeMismatch {
    val program = "function unit @main( ) { block %entry { return int32 12 } }"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def entryBlockWithParameters {
    val program = "function unit @main { block %entry(unit %u) { return () } }"
    programContainsError[EntryParametersException](program)
  }

  @Test
  def paramsInNonEntryBlockCorrect {
    val program = "function unit @main { block %entry { return () } }\n" +
                  "function unit @f(unit %x) {\n" +
                  "  block %b1 {\n" +
                  "    branch @f.b2( )\n" +
                  "  }\n" +
                  "  block %b2( ) {\n" +
                  "    return unit @f.x\n" +
                  "  }\n" +
                  "}\n"
    programIsCorrect(program)
  }

  @Test
  def mainWithParameters {
    val program = "function unit @main(unit %x) { block %entry { return () } }"
    programContainsError[MainNonEmptyParametersException](program)
  }

  @Test
  def mainReturnType {
    val program = "function int32 @main { block %entry { return int32 12 } }"
    programContainsError[MainReturnTypeException](program)
  }

  @Test
  def programMissingMain {
    val module = new Module(ty = ModuleType.PROGRAM)
    val errors = module.validateProgram
    containsError[MissingMainException](errors)
  }

  @Test
  def externalDefinition {
    val program = "function int32 @foo\n" +
                  "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    int32 %x = scall @foo( )\n" +
                  "    intrinsic exit(int32 %x)\n" +
                  "  }\n" +
                  "}\n"
    val module = compileString(program)
    val errors = module.validateProgram
    containsError[ExternalDefinitionException](errors)
  }

  @Test
  def blockWithParameterWithoutPredecessor {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "  block %b(unit %x) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[BlockPredecessorException](program)
  }

  @Test
  def entryBlockWithPredecessors {
    val program = "function unit @main {\n" +
                  "  block %entry {\n" +
                  "    branch @main.entry()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[EntryBlockPredecessorException](program)
  }

  @Test
  def catchBlockInDifferentFunction {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @tungsten.Exception %exn = upcast null\n" +
                  "    branch @f.cb(class @tungsten.Exception %exn)\n" +
                  "  }\n" +
                  "  block %cb(class @tungsten.Exception %exn) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  } catch @f.entry()\n" +
                  "}\n"
    programContainsError[ScopeException](program)
  }

  @Test
  def catchBlockReceiveExceptions {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb(class @tungsten.Object %exn) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[InvalidExceptionHandlerException](program)
  }

  @Test
  def catchBlockArgumentTypeMismatch {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb(())\n" +
                  "  block %cb(class @tungsten.Exception %exn, int64 %a) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def catchBlockArgumentCount {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb(())\n" +
                  "  block %cb(class @tungsten.Exception %exn) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[FunctionArgumentCountException](program)
  }

  @Test
  def catchArgumentScope {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    int64 %a = binop int64 2 + int64 3\n" +
                  "    return ()\n" +
                  "  } catch @f.cb(int64 %a)\n" +
                  "  block %cb(class @tungsten.Exception %exn, int64 %a) {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[ScopeException](program)
  }
}
