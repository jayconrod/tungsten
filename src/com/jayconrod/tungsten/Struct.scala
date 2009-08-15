package com.jayconrod.tungsten

import Utilities._

final class Struct(name: Symbol,
                   val fields: List[Symbol],
                   location: Location = Nowhere)
  extends Definition(name, location)
{
  override def toString = {
    "struct " + name + "{\n  " + joinStrings("\n  ", fields) + "\n}"
  }
}
