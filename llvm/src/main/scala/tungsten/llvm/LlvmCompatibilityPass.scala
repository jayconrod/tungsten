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

package tungsten.llvm

import scala.collection.immutable.TreeMap
import tungsten.Utilities._
import tungsten.{Symbol, SymbolFactory}

class LlvmCompatibilityPass
  extends tungsten.InternalPass
  with tungsten.InstructionRewritePass
{
  override def processModule(module: tungsten.Module): tungsten.Module = {
    val steps = List(addRuntime _,
                     processStrings _,
                     processChars _,
                     processInstructions _,
                     processMain _,
                     PhiConversion)
    val process = steps.reduceLeft(_.andThen(_))
    process(module)
  }

  def addRuntime(module: tungsten.Module): tungsten.Module = {
    val runtime = if (module.is64Bit) runtime64 else runtime32
    tungsten.Linker.linkModules(List(module, runtime),
                                module.name,
                                module.ty,
                                module.version,
                                module.filename,
                                module.dependencies,
                                module.searchPaths)

    // TODO: this will terminate with an error message. It should probably throw an 
    // exception instead.
  }

  def processMain(module: tungsten.Module): tungsten.Module = {
    module.get[tungsten.Function]("main") match {
      case Some(main) => {
        val blocks = module.getBlocks(main.blocks)
        val terminators = module.getInstructions(blocks.map(_.instructions.last))
        val returns = terminators.collect { case r: tungsten.ReturnInstruction => r }
        val processedReturns = returns.map(_.copyWith("value" -> tungsten.IntValue(0, 32)))
        val processedMain = main.copyWith("returnType" -> tungsten.IntType(32))
        module.replace((processedMain :: processedReturns): _*)
      }
      case None => module
    }
  }

  def processInstructions(module: tungsten.Module): tungsten.Module = {
    super[InstructionRewritePass].processModule(module)
  }

  def processStrings(module: tungsten.Module): tungsten.Module = {
    val strings = module.foldValues[Set[String]](Set(), collectStrings _)
    val stringGlobalMap = (Map[String, tungsten.Global]() /: strings) { (globals, string) =>
      globals + (string -> convertString(string))
    }
    val stringGlobals = stringGlobalMap.values.toSeq
    val stringMap = stringGlobalMap.mapValues(_.name)

    module.add(stringGlobals: _*).
           mapValues(convertStringValue(_, stringMap)).
           mapTypes(convertStringType _)
  }

  def collectStrings(strings: Set[String], value: tungsten.Value): Set[String] = {
    value match {
      case tungsten.StringValue(s) => strings + s
      case _ => strings
    }
  }    

  def convertString(string: String): tungsten.Global = {
    val storageType = tungsten.ArrayType(string.length, tungsten.IntType(16))
    val characters = string.toList.map { c => tungsten.IntValue(c.toLong, 16) }
    val storageValue = tungsten.ArrayValue(tungsten.IntType(16), characters)
    tungsten.Global(newName, storageType, Some(storageValue))
  } 

  def convertStringValue(value: tungsten.Value, 
                         stringMap: Map[String, Symbol]): tungsten.Value = 
  {
    value match {
      case tungsten.StringValue(s) => {
        assert(stringMap.contains(s))
        val storageType = tungsten.ArrayType(s.length, tungsten.IntType(16))
        val storageValue = tungsten.DefinedValue(stringMap(s), 
                                                 tungsten.PointerType(storageType))
        val storagePtr = tungsten.BitCastValue(storageValue, 
                                               tungsten.PointerType(tungsten.IntType(16)))
        tungsten.StructValue("tungsten.string",
                             List(storagePtr, tungsten.IntValue(s.length, 64)))
      }
      case _ => value
    }
  }
  def convertStringType(ty: tungsten.Type): tungsten.Type = {
    val stringType = tungsten.StructType("tungsten.string")
    if (ty eq tungsten.StringType)
      stringType
    else
      ty
  }

  def processChars(module: tungsten.Module): tungsten.Module = {
    module.mapValues(convertCharValue _).mapTypes(convertCharType _)
  }

  def convertCharValue(value: tungsten.Value): tungsten.Value = {
    value match {
      case tungsten.CharValue(c) => tungsten.IntValue(c.toLong, 16)
      case _ => value
    }
  }

  def convertCharType(ty: tungsten.Type): tungsten.Type = {
    ty match {
      case tungsten.CharType => tungsten.IntType(16)
      case _ => ty
    }
  }

  def rewriteInstruction(instruction: tungsten.Instruction,
                         block: tungsten.Block,
                         module: tungsten.Module): RewriteResult =
  {
    instruction match {
      case tungsten.AddressInstruction(name, ty, base, indices, _) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val cAddress = instruction.copyWith("indices" -> cIndices).asInstanceOf[tungsten.Instruction]
        RewrittenInstructions(casts :+ cAddress)
      }

      case tungsten.CatchInstruction(name, ty, _) => {
        val getExnPtr = tungsten.StaticCallInstruction(newName(name),
                                                       tungsten.PointerType(tungsten.IntType(8)),
                                                       Symbol("llvm.eh.exception"),
                                                       Nil, Nil)
        val personality = tungsten.BitCastInstruction(newName(name),
                                                      tungsten.PointerType(tungsten.IntType(8)),
                                                      tungsten.DefinedValue("__gxx_personality_v0",
                                                                            tungsten.PointerType(tungsten.FunctionType(tungsten.IntType(32),
                                                                                                                       Nil,
                                                                                                                       List(tungsten.VariadicType)))))
        val select = tungsten.StaticCallInstruction(newName(name),
                                                    tungsten.IntType(32),
                                                    Symbol("llvm.eh.selector"),
                                                    Nil,
                                                    List(getExnPtr.makeValue, 
                                                         personality.makeValue,
                                                         tungsten.BitCastValue(tungsten.NullValue,
                                                                               tungsten.PointerType(tungsten.IntType(8)))))
        val beginCatch = tungsten.StaticCallInstruction(newName(name),
                                                        tungsten.PointerType(tungsten.IntType(8)),
                                                        "__cxa_begin_catch",
                                                        Nil, List(getExnPtr.makeValue))
        val endCatch = tungsten.StaticCallInstruction(newName(name),
                                                      tungsten.UnitType,
                                                      "__cxa_end_catch",
                                                      Nil, Nil)
        RewrittenInstructions(List(getExnPtr, personality, select, beginCatch, endCatch))
      }

      case tungsten.ExtractInstruction(name, ty, value, indices, _) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val cExtract = instruction.copyWith("indices" -> cIndices).asInstanceOf[tungsten.Instruction]
        RewrittenInstructions(casts :+ cExtract)
      }

      case tungsten.HeapAllocateInstruction(name, ty, _) => {
        val size = ty.size(module).toInt
        val malloc = tungsten.StaticCallInstruction(newName(name),
                                                    tungsten.PointerType(tungsten.IntType(8)),
                                                    "tungsten.malloc",
                                                    Nil,
                                                    List(tungsten.IntValue(size, 32)))
        val cast = tungsten.BitCastInstruction(name,
                                               ty,
                                               malloc.makeValue)
        RewrittenInstructions(List(malloc, cast))
      }

      case tungsten.HeapAllocateArrayInstruction(name, ty, count, _) => {
        val elementSize = ty.size(module)
        val elementSizeVal = tungsten.IntValue(elementSize, tungsten.IntType.wordSize(module))
        val totalSize = tungsten.BinaryOperatorInstruction(newName(name),
                                                           tungsten.IntType.wordType(module),
                                                           tungsten.BinaryOperator.MULTIPLY,
                                                           count,
                                                           elementSizeVal)
        val malloc = tungsten.StaticCallInstruction(newName(name),
                                                    tungsten.PointerType(tungsten.IntType(8)),
                                                    "tungsten.malloc",
                                                    Nil,
                                                    List(totalSize.makeValue))
        val cast = tungsten.BitCastInstruction(name,
                                               ty,
                                               malloc.makeValue)
        RewrittenInstructions(List(totalSize, malloc, cast))
      }

      case tungsten.InsertInstruction(name, ty, value, base, indices, _) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val cInsert = instruction.copyWith("indices" -> cIndices).asInstanceOf[tungsten.Instruction]
        RewrittenInstructions(casts :+ cInsert)
      }

      case tungsten.IntrinsicCallInstruction(name, ty, intrinsic, arguments, anns) => {
        import tungsten.Intrinsic._
        val cTarget: Symbol = intrinsic match {
          case EXIT => "tungsten.exit"
          case READ => "tungsten.read"
          case WRITE => "tungsten.write"
          case OPEN => "tungsten.open"
          case CLOSE => "tungsten.close"
          case _ => throw new RuntimeException("Invalid intrinsic: " + intrinsic)
        }
        val scallInst = tungsten.StaticCallInstruction(name, ty, cTarget, Nil, arguments, anns)
        RewrittenInstructions(List(scallInst))
      }

      case tungsten.LoadElementInstruction(name, ty, base, indices, anns) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val address = tungsten.AddressInstruction(newName(name),
                                                  tungsten.PointerType(ty),
                                                  base,
                                                  cIndices)
        val load = tungsten.LoadInstruction(name, ty, address.makeValue, anns)
        RewrittenInstructions(casts ++ List(address, load))
      }

      case pcallInst: tungsten.PointerCallInstruction => rewriteCall(pcallInst, block, module)

      case tungsten.StackAllocateArrayInstruction(name, ty, count, _) => {
        val (cCount, cast) = convertWordTo32Bit(count, name)
        val cStack = instruction.copyWith("count" -> cCount).asInstanceOf[tungsten.Instruction]
        RewrittenInstructions(cast.toList :+ cStack)
      }

      case scallInst: tungsten.StaticCallInstruction => rewriteCall(scallInst, block, module)

      case tungsten.StoreElementInstruction(name, ty, value, base, indices, anns) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val address = tungsten.AddressInstruction(newName(name),
                                                  tungsten.PointerType(value.ty),
                                                  base,
                                                  cIndices)
        val store = tungsten.StoreInstruction(name, ty, value, address.makeValue, anns)
        RewrittenInstructions(casts ++ List(address, store))
      }

      case tungsten.ThrowInstruction(name, ty, value, anns) => {
        assert(value.ty.isPointer)
        val exnAlloc = tungsten.StaticCallInstruction(newName(name),
                                                      tungsten.PointerType(tungsten.IntType(8)),
                                                      "__cxa_allocate_exception",
                                                      Nil,
                                                      List(tungsten.IntValue(tungsten.IntType.wordSize(module)/8, 64)))
        val exnCast = tungsten.BitCastInstruction(newName(name),
                                                  tungsten.PointerType(value.ty),
                                                  exnAlloc.makeValue)
        val exnStore = tungsten.StoreInstruction(newName(name),
                                                 tungsten.UnitType,
                                                 value,
                                                 exnCast.makeValue)
        val rttiValue = tungsten.BitCastValue(tungsten.DefinedValue("_ZTIPv", 
                                                                    tungsten.PointerType(tungsten.PointerType(tungsten.IntType(8)))),
                                              tungsten.PointerType(tungsten.IntType(8)))
        val throwCall = tungsten.StaticCallInstruction(name,
                                                       tungsten.UnitType,
                                                       "__cxa_throw",
                                                       Nil,
                                                       List(exnAlloc.makeValue,
                                                            rttiValue,
                                                            tungsten.NullValue))
        val end = tungsten.UnreachableInstruction(newName(name), tungsten.UnitType)

        RewrittenInstructions(List(exnAlloc, exnCast, exnStore, throwCall, end))
      }

      case _ => RewrittenInstructions(List(instruction))
    }
  }

  def rewriteCall(callInst: tungsten.CallInstruction,
                  block: tungsten.Block,
                  module: tungsten.Module): RewriteResult =
  {
    block.catchBlock match {
      case Some((catchBlockName, catchBlockArguments)) => {
        // LLVM represents calls where exception handling is needed as invoke instructions,
        // which must be at the end of the block. For now, we split the block so that the
        // call will be the last instruction before a branch. TungstenToLlvmConverter will
        // recognize this and translate to an invoke.
        SplitBlock(Nil, Nil, module, { (splitBlockName, splitBlockArguments, splitModule) =>
          val branch = tungsten.BranchInstruction(symbolFactory(block.name + "branch$"),
                                                  tungsten.UnitType,
                                                  splitBlockName,
                                                  splitBlockArguments)
          RewrittenInstructions(List(callInst, branch))
        })
      }
      case None => RewrittenInstructions(List(callInst))
    }
  }

  def convertIndicesTo32Bit(indices: List[tungsten.Value],
                            sibling: Symbol): (List[tungsten.Value], List[tungsten.Instruction]) =
  {
    val conversions = indices.map(convertWordTo32Bit(_, sibling))
    val (convertedIndices, casts) = conversions.unzip
    (convertedIndices, casts.flatten)
  }

  def convertWordTo32Bit(word: tungsten.Value,
                         sibling: Symbol): (tungsten.Value, Option[tungsten.Instruction]) = 
  {
    word match {
      case tungsten.IntValue(v, _) => (tungsten.IntValue(v.toInt, 32), None)
      case tungsten.DefinedValue(name, tungsten.IntType(_)) => {
        val cast = tungsten.IntegerTruncateInstruction(newName(sibling),
                                                       tungsten.IntType(32),
                                                       word)
        (cast.makeValue, Some(cast))
      }
      case _ => throw new RuntimeException("invalid index: " + word)
    }
  }

  lazy val runtime64 = {
    val runtimeName = "tungsten-llvm.w"
    val input = getClass.getResourceAsStream(runtimeName)
    val reader = new java.io.InputStreamReader(input)
    tungsten.ModuleIO.readText(reader, runtimeName)
  }
  lazy val runtime32 = runtime64.copyWith(is64Bit = false)

  private def newName: Symbol = symbolFactory.symbol("llvmCompat")

  private def newName(sibling: Symbol): Symbol = {
    if (sibling.isSimple)
      newName
    else {
      val parent = sibling.parent
      symbolFactory.complexSymbol(parent.name :+ "llvmCompat")
    }
  }
}

object LlvmCompatibilityPass
  extends Function1[tungsten.Module, tungsten.Module]
{
  def apply(module: tungsten.Module) = {
    val pass = new LlvmCompatibilityPass
    pass(module)
  }
}

final case class TungstenPhiInstruction(name: Symbol,
                                        ty: tungsten.Type,
                                        bindings: List[(tungsten.Value, Symbol)])
  extends tungsten.ExtendedInstruction
{
  def annotations = Nil

  def operands = bindings.map(_._1)

  override def usedSymbols = operandSymbols ++ bindings.map(_._2)
}
