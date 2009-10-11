package tungsten

sealed abstract class Instruction(name: Symbol, location: Location)
  extends Definition(name, location)
  with TypedDefinition

final case class BranchInstruction(override name: Symbol, 
                                   target: Value,
                                   arguments: List[Value],
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = UnitType(location)
}

final case class ReturnInstruction(override name: Symbol,
                                   value: Value,
                                   override location: Location = Nowhere)
  extends Instruction(name, location)
{
  def ty(module: Module) = UnitType(location)
}
