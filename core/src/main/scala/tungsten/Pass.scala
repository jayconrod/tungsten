package tungsten

trait Pass 
  extends Function1[Module, Module]
{
  def name: String
  def description: String
  def apply(module: Module): Module
}
