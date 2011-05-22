; This program prints "Hello, world!\n" on stdout. It shows some usage of global
; variables both here, and in the runtime.

; Here we declare the characters that make up the string. We don't use the 
; built-in string type since there is currently no way to convert to UTF-8.
global [14 x int8] @hello = [14 x int8] {
  int8 72,
  int8 101,
  int8 108,
  int8 108,
  int8 111,
  int8 44,
  int8 32,
  int8 119,
  int8 111,
  int8 114,
  int8 108,
  int8 100,
  int8 33,
  int8 10
}

function unit @main {
  block %entry {
    ; Even though the global string is declared as [14 x int8], it is actually
    ; [14 x int8]*. So we use it to get the address of the first character. The
    ; address instruction works exactly like getelementptr in LLVM. The first
    ; argument is the base pointer. The second argument is an index into the 
    ; array that pointer points to. Since we are pointing to a single string,
    ; rather than an array of strings, this is 0. If we had multiple [14 x int8]
    ; values stored contiguously in memory, we could use this to access them.
    ; The third argument is the index of the byte within the string. If we wanted
    ; to just print "world!\n", we could use 7 instead of 0 here. 
    int8* %buffer = address [14 x int8]* @hello, int64 0, int64 0

    ; We want to write the string to the STDOUT file descriptor. The Tungsten
    ; runtime defines this (see core/src/resources/tungsten/runtime.w for other
    ; global definitions). Again, even though this is defined as int32, we treat
    ; it as int32*, so we need to load it.
    int32 %fd = load int32* @tungsten.STDOUT

    ; The write function is an intrinsic, since we can't write an implementation
    ; of it in Tungsten. When this code gets lowered to LLVM, intrinsic calls are
    ; transformed to normal function calls. The actual function is defined in
    ; llvm/runtime/runtime.ll. It just calls the write function from the C 
    ; standard library.
    int64 %bytesWritten = intrinsic write(int32 %fd, int8* %buffer, int64 14)

    return ()
  }
}

