function unit @main {
  block %entry {
    branch @main.throw()
  }
  block %throw {
    class @tungsten.Exception %exn = new @tungsten.Exception.ctor()
    throw class @tungsten.Exception %exn
  } catch @main.cb()
  block %cb {
    class @tungsten.Exception %exn = catch
    return ()
  }  
}
