package tungsten

import org.junit.Test
import org.junit.Assert._

class TypeTest {
  @Test
  def unitEquals = {
    val u1 = UnitType
    val u2 = UnitType
    assertTrue(u1 == u2)
    assertEquals(u1.hashCode, u2.hashCode)
  }

  @Test
  def intEquals = {
    val i1 = IntType(32)
    val i2 = IntType(32)
    assertTrue(i1 == i2)
    assertEquals(i1.hashCode, i2.hashCode)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def badInt = {
    val i = IntType(33)
    ()
  }

  @Test
  def floatEquals = {
    val f1 = FloatType(32)
    val f2 = FloatType(32)
    assertTrue(f1 == f2)
    assertEquals(f1.hashCode, f2.hashCode)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def badFloat = {
    val f = FloatType(16)
    ()
  }

  @Test
  def universalEquals = {
    val p = new Symbol("T")
    val u1 = UniversalType(List(p), UnitType())
    val u2 = UniversalType(List(p), UnitType())
    assertTrue(u1 == u2)
    assertEquals(u1.hashCode, u2.hashCode)
  }

  @Test
  def existentialEquals = {
    val p = new Symbol("T")
    val e1 = ExistentialType(List(p), UnitType())
    val e2 = ExistentialType(List(p), UnitType())
    assertTrue(e1 == e2)
    assertEquals(e1.hashCode, e2.hashCode)
  }

  @Test
  def varEquals = {
    val varName = new Symbol("T")
    val v1 = VariableType(varName)
    val v2 = VariableType(varName)
    assertTrue(v1 == v2)
    assertEquals(v1.hashCode, v2.hashCode)
  }

  @Test
  def arrayEquals = {
    val et = IntType(32)
    val a1 = ArrayType(None, et)
    val a2 = ArrayType(None, et)
    assertTrue(a1 == a2)
    assertEquals(a1.hashCode, a2.hashCode)
  }

  @Test
  def functionEquals = {
    val f1 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    val f2 = FunctionType(IntType(32), List(IntType(32), FloatType(32)))
    assertTrue(f1 == f2)
    assertEquals(f1.hashCode, f2.hashCode)
  }

  @Test
  def classEquals = {
    val className = new Symbol("Foo")
    val c1 = ClassType(className)
    val c2 = ClassType(className)
    assertTrue(c1 == c2)
    assertEquals(c1.hashCode, c2.hashCode)
  }

  @Test
  def interfaceEquals = {
    val interfaceName = new Symbol("Foo")
    val i1 = InterfaceType(interfaceName)
    val i2 = InterfaceType(interfaceName)
    assertTrue(i1 == i2)
    assertEquals(i1.hashCode, i2.hashCode)
  }

  @Test
  def nullEquals = {
    val n1 = NullType()
    val n2 = NullType()
    assertTrue(n1 == n2)
    assertEquals(n1.hashCode, n2.hashCode)
  }

  @Test
  def defaultSubtypeSelf = {
    val t1 = UnitType()
    val t2 = UnitType()
    assertTrue(t1 isSubtypeOf t2)
  }

  @Test
  def defaultSubtypeOther = {
    val t1 = UnitType()
    val t2 = IntType(32)
    assertFalse(t1 isSubtypeOf t2)
  }

  @Test
  def nullSubtypePointer = {
    val t1 = NullType()
    val t2 = PointerType(UnitType())
    assertTrue(t1 isSubtypeOf t2)
  }

  @Test
  def subtypeOperator = {
    val t1 = NullType()
    val t2 = PointerType(UnitType())
    assertTrue(t1 <<: t2)
  }
}
