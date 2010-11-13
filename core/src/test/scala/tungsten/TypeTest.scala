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
  def validateTypeArgumentsCorrect {
    val X = Class("X", List(T.name), Some(rootType), Nil, Nil, Nil, Nil, Nil, Nil)
    val m = module.add(X)
    val Xty = ClassType(X.name, List(ClassType(B.name)))
    assertIsCorrect(Xty.validateTypeArguments(m, Nowhere))
  }

  @Test
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
    assertContainsError[TypeArgumentCountException](Aty.validateTypeArgumentCount(module, Nowhere))
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

  @Test
  def classSubtypeSuper {
    val program = "class @A\n" +
                  "class @B <: class @A"
    val module = compileString(program)
    val a = ClassType("A")
    val b = ClassType("B")
    assertTrue(b.isSubtypeOf(a, module))
    assertFalse(a.isSubtypeOf(b, module))
  }

  @Test
  def classSubtypeInterface {
    val program = "class @A\n" +
                  "interface @I <: class @A\n" +
                  "class @B <: class @A {\n" +
                  "  interface @I\n" +
                  "}"
    val module = compileString(program)
    val a = ClassType("A")
    val i = InterfaceType("I")
    val b = ClassType("B")
    assertTrue(i.isSubtypeOf(a, module))
    assertTrue(b.isSubtypeOf(i, module))
  }

  @Test
  def invariantSubtype {
    val D = Class("D", List(T.name), None, Nil, Nil, Nil, Nil, Nil)
    val x = ClassType(D.name, List(ClassType(A.name)))
    val y = ClassType(D.name, List(ClassType(B.name)))
    val module = testModule(A, B, D, T)
    assertFalse(x.isSubtypeOf(y, module))
    assertFalse(y.isSubtypeOf(x, module))
  }

  @Test
  def covariantSubtype {
    val T = TypeParameter("T", None, None, Variance.COVARIANT)
    val D = Class("D", List(T.name), None, Nil, Nil, Nil, Nil, Nil)
    val x = ClassType(D.name, List(ClassType(A.name)))
    val y = ClassType(D.name, List(ClassType(B.name)))
    val module = testModule(A, B, D, T)
    assertTrue(y.isSubtypeOf(x, module))
  }

  @Test
  def contravariantSubtype {
    val T = TypeParameter("T", None, None, Variance.CONTRAVARIANT)
    val D = Class("D", List(T.name), None, Nil, Nil, Nil, Nil, Nil)
    val x = ClassType(D.name, List(ClassType(A.name)))
    val y = ClassType(D.name, List(ClassType(B.name)))
    val module = testModule(A, B, D, T)
    assertTrue(x.isSubtypeOf(y, module))
  }

  @Test
  def substituteVariable {
    val s = VariableType("S")
    val t = VariableType("T")
    assertEquals(t, s.substitute(s.variableName, t))
  }

  @Test
  def substituteClassType {
    val s = VariableType("S")
    val t = VariableType("T")
    val c = ClassType("C", List(s))
    val expected = ClassType(c.className, List(t))
    assertEquals(expected, c.substitute(s.variableName, t))
  }

  @Test
  def substituteInterfaceType {
    val s = VariableType("S")
    val t = VariableType("T")
    val i = InterfaceType("I", List(s))
    val expected = InterfaceType(i.interfaceName, List(t))
    assertEquals(expected, i.substitute(s.variableName, t))
  }

  @Test
  def substituteInheritedType {
    val T = TypeParameter("T", None, None, Variance.INVARIANT)
    val A = Class("A", List(T.name), None, Nil, Nil, Nil, Nil, Nil)
    val B = Class("B", Nil, Some(ClassType("A", List(ClassType("B")))), Nil, Nil, Nil, Nil, Nil)
    val module = testModule(T, A, B)

    val ty = A.selfType
    val expected = B.superclass.get
    assertEquals(expected, A.substituteInheritedType(ty, expected.typeArguments))
  }
}
