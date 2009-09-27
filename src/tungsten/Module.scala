package tungsten

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import Utilities._

final class Module {
  private val definitions: Map[Symbol, Definition] = new HashMap[Symbol, Definition]

  def add(defn: Definition) = {
    definitions.get(defn.name) match {
      case Some(d) => throw new RedefinedSymbolException(defn.name, defn.location, d.location)
      case None => definitions += defn.name -> defn
    }
  }

  def get(name: Symbol) = definitions.get(name)    
}
