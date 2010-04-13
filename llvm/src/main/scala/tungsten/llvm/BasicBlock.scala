package tungsten.llvm

final case class BasicBlock(name: String,
                            instructions: List[Instruction])
