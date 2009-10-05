package tungsten

sealed abstract class Instruction(name: Symbol, location: Location)
  extends Definition(name, location)

final case class BranchInstruction(override name: Symbol, 
                                   target: Value,
                                   arguments: List[Value],
                                   override location: Location = Nowhere)
  extends Instruction(name, location)

final case class ReturnInstruction(override name: Symbol,
                                   value: Value,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
