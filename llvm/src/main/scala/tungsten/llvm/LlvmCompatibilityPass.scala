package tungsten.llvm

import tungsten.Utilities._

object LlvmCompatibilityPass {
  def apply(module: tungsten.Module): tungsten.Module = processMain(module)

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
