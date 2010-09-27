package tungsten

import scala.collection.immutable.TreeMap
import org.junit.Test
import org.junit.Assert._
import Utilities._

class TypeTest {
  val clas = Class("tungsten.Object", Nil, None, Nil, Nil, Nil, Nil, Nil)
  val definitions = TreeMap(clas.name -> clas)
  val module = new Module(definitions = definitions)
  val rootTy = ClassType(clas.name)

  @Test
  def unitEquals {
    val u1 = UnitType
    val u2 = UnitType
    assertTrue(u1 == u2)
    assertEquals(u1.hashCode, u2.hashCode)
  }

  @Test
  def intEquals {
    val i1 = IntType(32)
    val i2 = IntType(32)
    assertTrue(i1 == i2)
    assertEquals(i1.hashCode, i2.hashCode)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def badInt {
    val i = IntType(33)
    ()
  }

  @Test
  def intMinMax {
    assertEquals(java.lang.Byte.MAX_VALUE, IntType(8).maxValue)
    assertEquals(java.lang.Byte.MIN_VALUE, IntType(8).minValue)
    assertEquals(java.lang.Long.MAX_VALUE, IntType(64).maxValue)
    assertEquals(java.lang.Long.MIN_VALUE, IntType(64).minValue)
  }

  @Test
  def floatEquals {
    val f1 = FloatType(32)
    val f2 = FloatType(32)
    assertTrue(f1 == f2)
    assertEquals(f1.hashCode, f2.hashCode)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def badFloat {
    val f = FloatType(16)
    ()
  }

  @Test
  def arrayEquals {
    val et = IntType(32)
    val a1 = ArrayType(1L, et)
    val a2 = ArrayType(1L, et)
    assertTrue(a1 == a2)
    assertEquals(a1.hashCode, a2.hashCode)
  }

  @Test
  def functionEquals {
    val f1 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    val f2 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    assertTrue(f1 == f2)
    assertEquals(f1.hashCode, f2.hashCode)
  }

  @Test
  def nullEquals {
    val n1 = NullType
    val n2 = NullType
    assertTrue(n1 == n2)
    assertEquals(n1.hashCode, n2.hashCode)
  }

  @Test
  def objectTypes {
    assertTrue(ClassType("@C").isObject)
    assertTrue(ClassType("@C").isPointer)
    assertTrue(InterfaceType("@I").isObject)
    assertTrue(InterfaceType("@I").isPointer)
    assertTrue(VariableType("@T").isObject)
    assertTrue(VariableType("@T").isPointer)
  }

  @Test
  def rootClassType {
    assertTrue(rootTy.isRootClassType(module))
  }

  @Test
  def defaultSubtypeSelf {
    val t1 = UnitType
    val t2 = UnitType
    assertTrue(t1.isSubtypeOf(t2, module))
  }

  @Test
  def defaultSubtypeOther {
    val t1 = UnitType
    val t2 = IntType(32)
    assertFalse(t1.isSubtypeOf(t2, module))
  }

  @Test
  def nullSubtypePointer {
    val t1 = NullType
    val t2 = PointerType(UnitType)
    assertTrue(t1.isSubtypeOf(t2, module))
  }

  @Test
  def unboundedVariableSubtypeRoot {
    val tyParam = TypeParameter("T", None, None)
    val tyVar = VariableType(tyParam.name)
    val m = module.add(tyParam)
    assertTrue(tyVar.isSubtypeOf(rootTy, m))
  }

  @Test
  def boundedVariableSubtypeRoot {
    val tyParam = TypeParameter("T", Some(rootTy), None)
    val tyVar = VariableType(tyParam.name)
    val m = module.add(tyParam)
    assertTrue(tyVar.isSubtypeOf(rootTy, m))
  }

  @Test
  def classSubtypeSelf {
    assertTrue(rootTy.isSubtypeOf(rootTy, module))
  }
}
