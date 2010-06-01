package tungsten.llvm

import tungsten.Utilities._
import tungsten.Symbol

object LlvmCompatibilityPass 
  extends Function1[tungsten.Module, tungsten.Module] 
{
  def apply(module: tungsten.Module): tungsten.Module = process(module) 

  val process = (processMain _) andThen PhiConversion

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
