package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._
import Linker._

class LinkerTest {
  @Test
  def parseLibraryTest {
    def test(libStr: String, 
             expectedName: String, 
             expectedMinVersion: String, 
             expectedMaxVersion: String) 
    {
      libStr match {
        case LibraryArgument(name, minVersion, maxVersion) => {
          assertEquals(expectedName, name)
          assertEquals(expectedMinVersion, minVersion)
          assertEquals(expectedMaxVersion, maxVersion)
        }
        case _ => fail("library string did not match: " + libStr)
      }
    }
    test("-lfoo", "foo", null, null)
    test("-lfoo.bar", "foo.bar", null, null)
    test("-lfoo:0.1", "foo", "0.1", "")
    test("-lfoo:0.1-", "foo", "0.1", "")
    test("-lfoo:-0.1", "foo", "", "0.1")
    test("-lfoo:0.1-2.3", "foo", "0.1", "2.3")
  }

  @Test
  def isStrongTest {
    assertFalse(isStrong(Function("f", Nil, UnitType(), Nil)))
    assertTrue(isStrong(Function("f", Nil, UnitType(), List("p"))))
    assertFalse(isStrong(Global("g", UnitType(), None)))
    assertTrue(isStrong(Global("g", UnitType(), Some(UnitValue()))))
    assertFalse(isStrong(Parameter("p", UnitType())))
    assertTrue(isStrong(Struct("s", Nil)))
  }
}
