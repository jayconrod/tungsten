; This is the most basic program you can write. If you link a collection of Tungsten
; modules as a program "w-link -t program", Tungsten expects for there to be a "main"
; function which takes no parameters and returns unit. When this function returns
; normally, 0 is returned to the OS. You can terminate the program with an error
; code by calling the "exit" intrinsic.

function unit @main {
  block %entry {
    return ()
  }
}
