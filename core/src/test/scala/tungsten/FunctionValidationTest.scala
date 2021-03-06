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
  override def compileProgram(program: String): Module = {
    linkRuntime(compileString(program))
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
    programContainsError[BlockTerminationException](program)
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
  def variadicArguments {
    val program = "function unit @f(int64 %x, ... %va)\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    scall @f(int64 12, int64 34)\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programIsCorrect(program)
  }

  @Test
  def nonVariadicArgumentsChecked {
    val program = "function unit @f(int64 %x, ... %va)\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    scall @f((), ())\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[TypeMismatchException](program)
  }

  @Test
  def variadicFewerArguments {
    val program = "function unit @f(int64 %x, ... %va)\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    scall @f()\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[FunctionArgumentCountException](program)
  }

  @Test
  def variadicFirstParameter {
    val program = "function unit @f(... %va, int64 %x)"
    programContainsError[VariadicFunctionException](program)
  }

  @Test
  def catchBlockInDifferentFunction {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb() {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n" +
                  "function unit @g {\n" +
                  "  block %entry {\n" +
                  "    branch @g.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    return ()\n" +
                   " } catch @f.cb()\n" +
                  "}\n"
    programContainsError[ScopeException](program)
  }

  @Test
  def catchBlock {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb() {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programIsCorrect(program)
  }

  @Test
  def catchBlockHasCatchBlock {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a() {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb() {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  } catch @f.cb2()\n" +
                  "  block %cb2() {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidCatchBlockException](program)
  }

  @Test
  def entryHasCatchBlock {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb() {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[InvalidCatchBlockException](program)
  }

  @Test
  def entryIsCatchBlock {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    return ()\n" +
                  "  } catch @f.entry()\n" +
                  "}"
    programContainsError[CatchEntryException](program)
  }

  @Test
  def branchToCatchBlock {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    branch @f.cb()\n" +
                  "  } catch @f.cb() \n" +
                  "  block %cb {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}\n"
    programContainsError[CatchBlockBranchException](program)
  }

  @Test
  def catchBlockWithoutCatch {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a()\n" +
                  "  }\n" +
                  "  block %a {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb()\n" +
                  "  block %cb {\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[InvalidCatchBlockException](program)
  }

  @Test
  def catchInstructionNotFirst {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    unit* %x = stack\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[CatchInstructionException](program)
  }

  @Test
  def catchBlockArgumentTypeMismatch {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb(())\n" +
                  "  block %cb(int64 %a) {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
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
                  "  block %cb() {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
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
                  "  block %cb(int64 %a) {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programContainsError[ScopeException](program)
  }

  @Test
  def catchArgumentScopeCorrect {
    val program = "function unit @f {\n" +
                  "  block %entry {\n" +
                  "    branch @f.a(int64 12)\n" +
                  "  }\n" +
                  "  block %a(int64 %x) {\n" +
                  "    return ()\n" +
                  "  } catch @f.cb(int64 @f.a.x)\n" +
                  "  block %cb(int64 %a) {\n" +
                  "    class @tungsten.Exception %exn = catch\n" +
                  "    return ()\n" +
                  "  }\n" +
                  "}"
    programIsCorrect(program)
  }
}
