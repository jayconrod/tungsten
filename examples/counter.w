; This example shows how to declare and use a simple class. Counter contains
; an integer, which is set in the constructor. Whenever the "next" method is
; called, it returns the integer and increments it. 
;
; Classes, interfaces, and type parameters are considered high level constructs.
; There is a transformation that converts these definitions into lower level
; representations. Try the following command:
;     w-as <counter.w | w-opt --lower | w-dis

; A class definition has many parts:
; - class name
; - type parameters (not included here)
; - superclass type (tungsten.Object is the root class)
; - lists of interface types and methods (not included here)
; - list of constructors
; - list of methods
; - fields
; Note that constructors and methods are referenced symbolically; they are not
; included in the class definition. This allows the same function to be used
; as a method in multiple classes (as is the case in inheritance). The order of
; methods and fields is significant, since we refer to them by index rather than
; by name most of the time.
class @Counter <: class @tungsten.Object {
  constructors { %ctor, %setCtor }
  methods { %next, %set }
  field int64 %count
}

; A constructor is a normal function. Its first parameter is an object of the
; class it is constructing. Its return type must be unit.
function unit @Counter.ctor(class @Counter %this) {
  block %entry {
    ; loadelement and storeelement are used to get and set fields in an object.
    ; The first operand is the value being stored. The second operand is a pointer
    ; to the object. The remaining operands are indices. We just use one index
    ; here to refer to the first field in the object. If the field were an aggregate
    ; (struct or fixed size array), we could use additional indices to refer to 
    ; elements within the aggregate.
    storeelement int64 0, class @Counter @Counter.ctor.this, int64 0
    return ()
  }
}

function unit @Counter.setCtor(class @Counter %this, int64 %start) {
  block %entry {
    storeelement int64 @Counter.setCtor.start, class @Counter @Counter.setCtor.this, int64 0
    return ()
  }
}

; Methods are similar to constructors in that they are normal functions. The 
; first parameter is a pointer to an object they belong to. However, the type
; of the first parameter doesn't have to be of the same class; it could be of
; a superclass if the method is inherited.
function int64 @Counter.next(class @Counter %this) {
  block %entry {
    int64 %count = loadelement class @Counter @Counter.next.this, int64 0
    int64 %nextCount = binop int64 %count + int64 1

    ; vcall is used to make a virtual call on a method. The first operand is
    ; the object we are calling a method on. After this (followed by a colon)
    ; is the method index. 1 refers to the second method, "set" here. We need 
    ; to refer to methods by index since we can't always know the actual class
    ; of an object and what functions implement its methods.
    vcall class @Counter @Counter.next.this:1(int64 %nextCount)
    return int64 %count
  }
}

function unit @Counter.set(class @Counter %this, int64 %newCount) {
  block %entry {
    storeelement int64 @Counter.set.newCount, class @Counter @Counter.set.this, int64 0
    return ()
  }
}

function unit @main {
  block %entry {
    ; The "new" instruction is used to instantiate a class. This does three things:
    ; 1. allocates memory on the heap for the object
    ; 2. stores a pointer to the class's vtable in the object
    ; 3. calls the specified constructor
    ; The result of a "new" instruction is the allocated object.
    class @Counter %ctr = new @Counter.ctor()
    branch @main.loop(class @Counter %ctr)
  }
  block %loop(class @Counter %ctr) {
    int64 %next = vcall class @Counter %ctr:0()
    boolean %cmp = relop int64 %next < int64 10
    cond boolean %cmp ? @main.loop(class @Counter %ctr) : @main.exit(int64 %next)
  }
  block %exit(int64 %final) {
    int32 %code = itruncate int64 %final
    intrinsic exit(int32 %code)
  }
}
