function unit @main {
  block %entry {
    branch @main.cb()
  }
  block %cb {
    class @tungsten.Exception %exn = catch
    return ()
  }  
}
