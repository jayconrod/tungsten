global [18 x int8] @message = "caught exception!\0a"

; We need to declare system calls we use. The actual system calls are
; done by libc, so we can declare and call these like normal functions.
function int32 @write(int32 %fd, int8* %buffer, int32 %size)

; We define other constants globally as well
global int32 @STDOUT = int32 1

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
    int32 %ret = scall @write(int32 1, int8* %msg, int32 18)
    return ()
  }
}
