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

; We need to declare system calls we use. The actual system calls are
; done by libc, so we can declare and call these like normal functions.
function int32 @write(int32 %fd, int8* %buffer, int32 %size)

; We define other constants globally as well
global int32 @STDOUT = int32 1

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

    ; We want to write the string to the STDOUT file descriptor. STDOUT is a 
    ; global variable, so we load it here.
    int32 %fd = load int32* @STDOUT

    ; To write the string out to the terminal, we call the libc function "write". 
    ; Tungsten has no built-in way to write data to a file.
    int32 %bytesWritten = scall @write(int32 %fd, int8* %buffer, int32 14)

    return ()
  }
}

