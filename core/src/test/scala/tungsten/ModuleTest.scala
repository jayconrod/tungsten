package tungsten

import Utilities._
import org.junit.Test
import org.junit.Assert._

class ModuleTest {
  val global = Global("g", IntType(8), Some(IntValue(2, 8)))
  val definitions = Map(global.name -> global)
  val module = new Module(definitions=definitions)

  def collect[T](s: Set[T], e: T): Set[T] = s + e

  def testMap(expected: Definition, module: Module) {
    val expectedDefinitions = Map(expected.name -> expected)
    val expectedModule = new Module(definitions=expectedDefinitions)
    assertEquals(expectedModule, module)
  }

  @Test
  def mapSymbols {
    def f(symbol: Symbol): Symbol = "h"
    val expected = Global("h", global.ty, global.value)
    testMap(expected, module.mapSymbols(f))
  }

  @Test
  def mapValues {
    def addTwo(value: Value): Value = {
      value match {
        case IntValue(v, w) => IntValue(v + 2, w)
        case _ => value
      }
    }
    val expected = Global("g", IntType(8), Some(IntValue(4, 8)))
    testMap(expected, module.mapValues(addTwo))
  }

  @Test
  def mapTypes {
    def widen(ty: Type): Type = {
      ty match {
        case IntType(w) => IntType(w * 2)
        case _ => ty
      }
    }
    val expected = Global("g", IntType(16), Some(IntValue(2, 8)))
    testMap(expected, module.mapTypes(widen))
  }

  @Test
  def foldSymbols {
    assertEquals(Set(global.name), module.foldSymbols[Set[Symbol]](Set(), collect))
  }

  @Test
  def foldValues {
    assertEquals(Set(global.value.get), module.foldValues[Set[Value]](Set(), collect))
  }

  @Test
  def foldTypes {
    assertEquals(Set(global.ty), module.foldTypes[Set[Type]](Set(), collect))
  }
}
