class @Counter <: class @tungsten.Object {
  constructors { %ctor, %setCtor }
  methods { %next, %set }
  field int64 %count
}

function unit @Counter.ctor(class @Counter %this) {
  block %entry {
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

function int64 @Counter.next(class @Counter %this) {
  block %entry {
    int64 %count = loadelement class @Counter @Counter.next.this, int64 0
    int64 %nextCount = binop int64 %count + int64 1
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
