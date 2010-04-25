package tungsten

import org.junit.Test
import org.junit.Assert._
import Utilities._

class DefinitionTest {
  @Test
  def extractLocation {
    val defn = Global("foo", UnitType, None, 
                      List(AnnotationValue("tungsten.Location",
                                           List(StringValue("foo.w"),
                                                IntValue(1, 32),
                                                IntValue(2, 32),
                                                IntValue(3, 32),
                                                IntValue(4, 32)))))
    assertEquals(Location("foo.w", 1, 2, 3, 4), defn.getLocation)
  }
}
