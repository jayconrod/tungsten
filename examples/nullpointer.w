global [18 x int8] @message = "caught exception!\0a"

function unit @main {
  block %entry {
    branch @main.bb()
  }
  block %bb {
    class? @tungsten.Object %a = upcast null
    class @tungsten.Object %b = nullcheck class? @tungsten.Object %a
    intrinsic exit(int32 1)
    unreachable
  } catch @main.cb()
  block %cb {
    class @tungsten.Exception %exn = catch
    int8* %msg = address [18 x int8]* @message, int64 0, int64 0
    int64 %ret = intrinsic write(int32 1, int8* %msg, int64 18)
    return ()
  }
}
