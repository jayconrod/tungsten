package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

case class MappingFoo(name: Symbol, value: Value, ty: Type)
  extends Mapping[MappingFoo]

case class MappingBar[T](foos: List[T])
  extends Mapping[MappingBar[T]]

class MappingTest {
  def addTwo(oldValue: Value): Value = {
    oldValue match {
      case IntValue(i, w) => IntValue(i + 2, w)
      case _ => oldValue
    }
  }

  def sum(total: Long, value: Value): Long = {
    value match {
      case IntValue(v, _) => total + v
      case _ => total
    }
  }

  @Test
  def mapFieldsTest {
    val a = MappingFoo("a", IntValue(3, 32), IntType(32))
    def function(field: java.lang.reflect.Field, oldValue: AnyRef) = {
      oldValue match {
        case IntValue(i, w) => IntValue(i + 2, w)
        case _ => oldValue
      }
    }
    assertEquals(MappingFoo("a", IntValue(5, 32), IntType(32)), a.mapFields(function))
  }

  @Test
  def mapValuesTest {
    val a = MappingFoo("a", IntValue(3, 32), IntType(32))
    assertEquals(MappingFoo("a", IntValue(5, 32), IntType(32)), a.mapValues(addTwo))
  }

  @Test
  def mapValuesList {
    val a = MappingBar(List(IntValue(3, 32), IntValue(5, 32)))
    assertEquals(MappingBar(List(IntValue(5, 32), IntValue(7, 32))), a.mapValues(addTwo))
  }

  @Test
  def mapValuesRecursive {
    val a = MappingBar(List(MappingFoo("a", IntValue(3, 32), IntType(32))))
    val expected = MappingBar(List(MappingFoo("a", IntValue(5, 32), IntType(32))))
    assertEquals(expected, a.mapValues(addTwo))
  }

  @Test
  def mapValuesInternal {
    val array = ArrayValue(ArrayType(1, IntType(64)), 
                           List(ArrayValue(IntType(64), List(IntValue(0, 64)))))
    val foo = MappingFoo("a", array, UnitType)
    val arrayExpected = ArrayValue(ArrayType(1, IntType(64)),
                                   List(ArrayValue(IntType(64), List(IntValue(2, 64)))))
    val expected = MappingFoo("a", arrayExpected, UnitType)
    assertEquals(expected, foo.mapValues(addTwo _))
  }

  @Test
  def mapTypesInternal {
    def widen(ty: Type): Type = {
      ty match {
        case IntType(w) => IntType(w * 2)
        case _ => ty
      }
    }
    val ty = PointerType(IntType(8))
    val foo = MappingFoo("a", UnitValue, ty)
    val expectedTy = PointerType(IntType(16))
    val expectedFoo = MappingFoo("a", UnitValue, expectedTy)
    assertEquals(expectedFoo, foo.mapTypes(widen _))
  }

  @Test
  def mapValuesSelf {
    val value = IntValue(1, 64)
    val expected = IntValue(3, 64)
    assertEquals(expected, value.mapValues(addTwo _))
  }

  @Test
  def mapValuesOption {
    val global = Global("a", IntType(64), Some(IntValue(2, 64)))
    val expected = Global("a", IntType(64), Some(IntValue(4, 64)))
    assertEquals(expected, global.mapValues(addTwo _))
  }

  @Test
  def foldValues {
    val value = ArrayValue(IntType(64), List(IntValue(2, 64), IntValue(2, 64)))
    assertEquals(4L, value.foldValues(0L, sum _))
  }

  @Test
  def foldValuesSelf {
    val value = IntValue(3, 64)
    assertEquals(3L, value.foldValues(0L, sum _))
  }

  @Test
  def foldValuesOption {
    val global = Global("a", IntType(64), Some(IntValue(2, 64)))
    assertEquals(2L, global.foldValues(0L, sum _))
  }
}
