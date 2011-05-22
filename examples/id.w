; This example demonstrates type parameters for functions. Type parameters are
; similar to generics in Java or templates in C++. They let you declare a function
; or class without specifying its exact type. Instead, you define a number of 
; type parameters, which are filled in by the caller. In Tungsten, values whose
; type is a type parameter are treated as pointers to objects. 

; The id (identity) function always returns its argument. Since it doesn't actually
; examine its argument, the type shouldn't matter. We don't want to define 
; separate id functions for every type we want to process, so we parameterize it
; with "type %T". 
;
; Note that the id function can still only handle class and variable types; you
; still can't pass it an integer, array, struct, or other primitive type.
function type %T @id[type %T](type %T %x) {
  block %entry {
    return type @id.T @id.x
  }
}

class @A <: class @tungsten.Object {
  constructors { %ctor }
}

class @B <: class @tungsten.Object {
  constructors { %ctor }
}

function unit @A.ctor(class @A %this) {
  block %entry {
    return ()
  }
}

function unit @B.ctor(class @B %this) {
  block %entry {
    return ()
  }
}

function unit @main {
  block %entry {
    class @A %a = new @A.ctor()
    class @B %b = new @B.ctor()
    class @A %x = scall @id[class @A](class @A %a)
    class @B %y = scall @id[class @B](class @B %b)
    return ()
  }
}
