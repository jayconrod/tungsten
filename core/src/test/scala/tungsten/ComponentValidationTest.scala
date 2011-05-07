package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class ComponentValidationTest
  extends ValidationTest
{
  @Test
  def undefinedParameterType {
    val program = "function unit @main(class @A %x)"
    programContainsError[UndefinedSymbolException](program)
  }

  @Test
  def undefinedTypeParameterBound {
    val program = "function unit @f[type %T <: class @R](type %T %x)"
    programContainsError[UndefinedSymbolException](program)
  }

  @Test
  def duplicateComponent {
    val (instName, blockName) = (Symbol("ret"), Symbol("block"))
    val inst = ReturnInstruction(instName, UnitType, UnitValue)
    val block = Block(blockName, Nil, List(instName, instName))
    val function = Function(Symbol("main"), UnitType, Nil, Nil, List(blockName))
    var module = (new Module).add(inst, block, function)
    containsError[DuplicateComponentException](module.validate)
  }
}
