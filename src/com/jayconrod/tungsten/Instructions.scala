package com.jayconrod.tungsten

sealed abstract class Instruction(name: Symbol, location: Location = Nowhere)
  extends Definition(name, location)
