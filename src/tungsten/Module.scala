package tungsten

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import Utilities._

final class Module {
  private val definitions: Map[Symbol, Definition] = new HashMap[Symbol, Definition]
}
