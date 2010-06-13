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
    val mallocParam = tungsten.Parameter("tungsten.malloc.size", tungsten.IntType(32))
    val malloc = tungsten.Function("tungsten.malloc", tungsten.PointerType(tungsten.IntType(8)), List(mallocParam.name), Nil)
    module.add(mallocParam, malloc)
  }

  def processMain(module: tungsten.Module): tungsten.Module = {
    module.get[tungsten.Function]("main") match {
      case Some(main) => {
        val blocks = module.getBlocks(main.blocks)
        val terminators = module.getInstructions(blocks.map(_.instructions.last))
        val returns = terminators.collect { case r: tungsten.ReturnInstruction => r }
        val processedReturns = returns.map(_.copyWith("value" -> tungsten.IntValue(0, 32)))
        val processedMain = main.copyWith("returnType" -> tungsten.IntType(32))
        module.replace(processedMain :: processedReturns)
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
      case tungsten.HeapAllocateInstruction(name, ty, _) => {
        val size = ty.size(module).toInt
        val malloc = tungsten.StaticCallInstruction(newName(name),
                                                    tungsten.PointerType(tungsten.IntType(8)),
                                                    "tungsten.malloc",
                                                    List(tungsten.IntValue(size, 32)))
        val cast = tungsten.BitCastInstruction(name,
                                               ty,
                                               tungsten.DefinedValue(malloc.name, malloc.ty))
        List(malloc, cast)
      }
      case _ => List(instruction)
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
