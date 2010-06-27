package tungsten.llvm

import tungsten.Utilities._
import tungsten.Symbol

class LlvmCompatibilityPass
  extends Function1[tungsten.Module, tungsten.Module] 
{
  val symbolFactory = new tungsten.SymbolFactory

  def apply(module: tungsten.Module): tungsten.Module = process(module) 

  val process: tungsten.Module => tungsten.Module = {
    addRuntime _            andThen
      processInstructions _ andThen
      processMain _         andThen 
      PhiConversion
  }

  def addRuntime(module: tungsten.Module): tungsten.Module = {
    var cModule = module

    val noreturn = tungsten.Annotation("tungsten.NoReturn", Nil)
    cModule = cModule.replace(noreturn)

    val stringCharacters = tungsten.Field("tungsten.string.characters",
                                          tungsten.PointerType(tungsten.IntType(16)))
    val stringLength = tungsten.Field("tungsten.string.length", tungsten.IntType(64))
    val string = tungsten.Struct("tungsten.string", 
                                 List(stringCharacters.name, stringLength.name))
    cModule = cModule.replace(stringCharacters, stringLength, string)

    val mallocParam = tungsten.Parameter("tungsten.malloc.size", tungsten.IntType(32))
    val malloc = tungsten.Function("tungsten.malloc", 
                                   tungsten.PointerType(tungsten.IntType(8)), 
                                   List(mallocParam.name), 
                                   Nil)
    cModule = cModule.replace(mallocParam, malloc)

    val exitParam = tungsten.Parameter("tungsten.exit.code", tungsten.IntType(32))
    val exit = tungsten.Function("tungsten.exit", 
                                 tungsten.UnitType, 
                                 List(exitParam.name),
                                 Nil,
                                 List(tungsten.AnnotationValue("tungsten.NoReturn", Nil)))
    cModule = cModule.replace(exitParam, exit)

    cModule    
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
    val blocks = module.definitions.values.collect { case b: tungsten.Block => b }
    (module /: blocks) { (module, block) =>
      val instructions = module.getInstructions(block.instructions)
      val processedInstructions = instructions.flatMap(convertInstruction(_, module))
      val processedBlock = block.copyWith("instructions" -> processedInstructions.map(_.name))
      module.remove(block.name :: block.instructions).
        add(processedBlock :: processedInstructions)
    }
  }

  def convertInstruction(instruction: tungsten.Instruction,
                         module: tungsten.Module): List[tungsten.Instruction] = 
  {
    instruction match {
      case tungsten.AddressInstruction(name, ty, base, indices, _) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val cAddress = instruction.copyWith("indices" -> cIndices).asInstanceOf[tungsten.Instruction]
        casts :+ cAddress
      }

      case tungsten.ExtractInstruction(name, ty, value, indices, _) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val cExtract = instruction.copyWith("indices" -> cIndices).asInstanceOf[tungsten.Instruction]
        casts :+ cExtract
      }

      case tungsten.HeapAllocateInstruction(name, ty, _) => {
        val size = ty.size(module).toInt
        val malloc = tungsten.StaticCallInstruction(newName(name),
                                                    tungsten.PointerType(tungsten.IntType(8)),
                                                    "tungsten.malloc",
                                                    List(tungsten.IntValue(size, 32)))
        val cast = tungsten.BitCastInstruction(name,
                                               ty,
                                               malloc.makeValue)
        List(malloc, cast)
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
                                                    List(totalSize.makeValue))
        val cast = tungsten.BitCastInstruction(name,
                                               ty,
                                               malloc.makeValue)
        List(totalSize, malloc, cast)
      }

      case tungsten.InsertInstruction(name, ty, value, base, indices, _) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val cInsert = instruction.copyWith("indices" -> cIndices).asInstanceOf[tungsten.Instruction]
        casts :+ cInsert
      }

      case tungsten.IntrinsicCallInstruction(name, ty, intrinsic, arguments, anns) => {
        import tungsten.Intrinsic._
        val cTarget: Symbol = intrinsic match {
          case EXIT => "tungsten.exit"
          case _ => throw new RuntimeException("Invalid intrinsic: " + intrinsic)
        }
        List(tungsten.StaticCallInstruction(name, ty, cTarget, arguments, anns))
      }

      case tungsten.LoadElementInstruction(name, ty, base, indices, anns) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val address = tungsten.AddressInstruction(newName(name),
                                                  tungsten.PointerType(ty),
                                                  base,
                                                  cIndices)
        val load = tungsten.LoadInstruction(name, ty, address.makeValue, anns)
        casts ++ List(address, load)
      }

      case tungsten.StackAllocateArrayInstruction(name, ty, count, _) => {
        val (cCount, cast) = convertWordTo32Bit(count, name)
        val cStack = instruction.copyWith("count" -> cCount).asInstanceOf[tungsten.Instruction]
        cast.toList :+ cStack
      }

      case tungsten.StoreElementInstruction(name, ty, value, base, indices, anns) => {
        val (cIndices, casts) = convertIndicesTo32Bit(indices, name)
        val address = tungsten.AddressInstruction(newName(name),
                                                  tungsten.PointerType(ty),
                                                  base,
                                                  cIndices)
        val store = tungsten.StoreInstruction(name, ty, value, address.makeValue, anns)
        casts ++ List(address, store)
      }

      case _ => List(instruction)
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

final case class BitCastValue(value: tungsten.Value, ty: tungsten.Type)
  extends tungsten.ExtendedValue
