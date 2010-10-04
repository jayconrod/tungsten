package tungsten

import scala.collection.immutable.TreeMap
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import Utilities._

class TypeTest {
  def makeClass(name: Symbol, supertype: ClassType): Class = {
    Class(name, Nil, Some(supertype), Nil, Nil, Nil, Nil, Nil)
  }

  def makeInterface(name: Symbol, supertype: ObjectType): Interface = {
    Interface(name, Nil, supertype, Nil, Nil, Nil, Nil)
  }

  def assertContainsError[T <: CompileException](errors: List[CompileException])
                                                (implicit m: Manifest[T]) =
  {
    assertTrue(errors.exists(m.erasure.isInstance _))
  }

  def assertIsCorrect(errors: List[CompileException]) = {
    assertEquals(Nil, errors)
  }

  val rootClass = Class("tungsten.Object", Nil, None, Nil, Nil, Nil, Nil, Nil)
  val rootType = ClassType(rootClass.name)
  val A = makeClass("A", rootType)
  val B = makeClass("B", ClassType(A.name))
  val C = makeClass("C", ClassType(B.name))
  val T = TypeParameter("T", Some(ClassType(A.name)), Some(ClassType(C.name)), Variance.INVARIANT)
  val module = testModule(rootClass, A, B, C, T)

  @Test
  @Ignore
  def validateTypeArgumentsCorrect {
    val X = Class("X", List(T.name), Some(rootType), Nil, Nil, Nil, Nil, Nil, Nil)
    val m = module.add(X)
    val Xty = ClassType(X.name, List(ClassType(B.name)))
    assertIsCorrect(Xty.validateTypeArguments(m, Nowhere))
  }

  @Test
  @Ignore
  def validateTypeArgumentsBounds {
    val D = makeClass("D", ClassType(C.name))
    val X = Class("X", List(T.name), Some(rootType), Nil, Nil, Nil, Nil, Nil, Nil)
    val m = module.add(D, X)
    val Xhigh = ClassType(X.name, List(rootType))
    assertContainsError[TypeArgumentBoundsException](Xhigh.validateTypeArguments(m, Nowhere))
    val Xlow = ClassType(X.name, List(ClassType(D.name)))
    assertContainsError[TypeArgumentBoundsException](Xlow.validateTypeArguments(m, Nowhere))
  }

  @Test
  def validateTypeArgumentsCount {
    val Aty = ClassType(A.name, List(rootType))
    assertContainsError[TypeArgumentCountException](Aty.validateTypeArguments(module, Nowhere))
  }

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
    assertTrue(rootType.isRootClassType(module))
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
    val S = TypeParameter("S", None, None, Variance.INVARIANT)
    val Sty = VariableType(S.name)
    val m = module.add(S)
    assertTrue(Sty.isSubtypeOf(rootType, m))
  }

  @Test
  def boundedVariableSubtypeRoot {
    val S = TypeParameter("S", Some(rootType), None, Variance.INVARIANT)
    val Sty = VariableType(S.name)
    val m = module.add(S)
    assertTrue(Sty.isSubtypeOf(rootType, m))
  }

  @Test
  def classSubtypeSelf {
    assertTrue(rootType.isSubtypeOf(rootType, module))
  }
}
