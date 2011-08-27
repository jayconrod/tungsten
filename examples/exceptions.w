function unit @f {
  block %entry {
    class @tungsten.Exception %exn = new @tungsten.Exception.ctor()
    throw class @tungsten.Exception %exn
  }
}

function unit @main {
  block %entry {
    branch @main.a()
  }
  block %a {
    scall @f()
    return ()
  } catch @main.cb()
  block %cb {
    return ()
  }  
}
