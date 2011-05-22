; This is a program that prints its source code by reading its source file and
; echoing the contents to STDOUT. This demonstrates how to work with files and
; loops.

; Here we declare the filename. It must be a NUL-terminated UTF-8 string.
global [8 x int8] @filename = [8 x int8] {
  int8 102,
  int8 105,
  int8 108,
  int8 101,
  int8 115,
  int8 46,
  int8 119,
  int8 0
}

function unit @main {
  block %entry {
    ; address is used to get the address of the first character of the filename.
    ; See hello.w for a deeper explanation.
    int8* %filename = address [8 x int8]* @filename, int64 0, int64 0    

    int32 %flags = load int32* @tungsten.O_RDONLY

    ; The open intrinsic returns a file descriptor (int32) which we can read from.
    ; This works just like POSIX open.
    int32 %in = intrinsic open(int8* %filename, int32 %flags)

    ; We will be writing to STDOUT, so we load the file descriptor which is declared
    ; in the Tungsten runtime. We don't need the file descriptor in this block, 
    ; but loading it here means we don't have to load it in a loop.
    int32 %out = load int32* @tungsten.STDOUT

    ; The stackarray instruction allocates a buffer of any size on the stack. It
    ; returns a pointer to the first element. Since we know the size of the buffer,
    ; we could also have allocated with "[512 x int8]* %buffer = stack". However,
    ; fixed sized arrays can't be directly passed to read/write, so we would need
    ; to cast it with "address". It's easier to just use "stackarray".
    int8* %buffer = stackarray int64 512

    ; read will fill our buffer with up to 512 bytes of data. The number of bytes
    ; actually read will be returned. 0 is returned if we have hit the end of the
    ; file.
    int64 %bytesRead = intrinsic read(int32 %in, int8* %buffer, int64 512)

    ; A branch is like a goto: this just sends control to the beginning of the loop.
    ; Live-out variables are passed as parameters here.
    branch @main.loopCond(int32 %in, int32 %out, int8* %buffer, int64 %bytesRead)
  }

  ; Blocks in Tungsten must have one parameter for each live-in variable. Variables
  ; from different blocks cannot be accessed directly. However, we can still access
  ; parameters from the function.
  block %loopCond(int32 %in, int32 %out, int8* %buffer, int64 %bytesRead) {
    ; The relop instruction performs comparisons between two values. <, <=, >, >=
    ; can compare numeric values. ==, and != can compare any type of value.
    boolean %cmp = relop int64 %bytesRead > int64 0

    ; The cond instruction is used to implement if-statements and loops. If the 
    ; condition is true (we read bytes from the file), we branch to the loop body.
    ; Otherwise, we branch to the exit.
    cond boolean %cmp ? @main.loopBody(int32 %in, int32 %out, int8* %buffer, int64 %bytesRead)
                      : @main.exit(int32 %in)
  }
  block %loopBody(int32 %in, int32 %out, int8* %buffer, int64 %bytesRead) {
    int64 %bytesWritten = intrinsic write(int32 %out, int8* %buffer, int64 %bytesRead)
    int64 %bytesRead#1 = intrinsic read(int32 %in, int8* %buffer, int64 512)
    branch @main.loopCond(int32 %in, int32 %out, int8* %buffer, int64 %bytesRead#1)
  }
  block %exit(int32 %in) {
    intrinsic close(int32 %in)
    return ()
  }
}
