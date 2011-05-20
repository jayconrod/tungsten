; This example shows a simple iterative factorial function being called from the
; main function. It is probably the simplest program where we can show some
; interesting syntax.
;
; First, look at the function signature for @fac. It says the function returns
; an int64 and takes an int64 parameter. All named values in Tungsten are 
; prefixed by their types. This makes the language very verbose and simplifies
; validation.
;
; Named values always begin with @ or %. Names beginning with @ are absolute 
; names. Names beginning with % are relative to the definition in which they
; occur.
function int64 @fac(int64 %n) {
  ; Every function in Tungsten contains a list of basic blocks. Each block is
  ; a list of instructions which can be executed in sequence. If a function has
  ; no blocks, it is considered an "extern" function, defined elsewhere. The 
  ; first block is always the entry point (the name is not significant).
  block %entry {
    ; Here we branch to the beginning of the loop, giving the initial values for
    ; our induction variables. Note that we cannot refer to @fac's %n parameter
    ; directly. If we wrote %n, it would be expanded to @fac.entry.n instead of
    ; @fac.n. Tungsten has no concept of scope. Names are always explicit.
    branch @fac.loop_cond(int64 @fac.n, int64 1)
  }

  ; This block has two parameters, %n and %p. The %n is different from @fac.n.
  ; This one expands to @fac.loop_cond.n.
  block %loop_cond(int64 %n, int64 %p) {
    ; The relop instruction performs relational operations on two inputs. 
    ; <, >, <=, >= only work on numeric inputs. ==, != work on anything.
    ; The result is always a boolean.
    boolean %c = relop int64 %n != int64 0

    ; cond branches to one of two blocks depending on the given condition.
    cond boolean %c ? @fac.loop_body(int64 %n, int64 %p) : @fac.exit(int64 %p)
  }

  block %loop_body(int64 %n, int64 %p) {
    ; Tungsten has an SSA (single static assignment) representation. This means
    ; that we can never change the value of a local variable or parameter. 
    ; Instead, we just define a new value. 
    int64 %p#1 = binop int64 %n * int64 %p

    ; Note that Tungsten symbols have an optional numeric ID at the end. This 
    ; lets us have distinct variables with the same name. All definitions in
    ; Tungsten have unique symbols. So no other function could define an 
    ; instructions @fac.loop_body.n#1 because it would collide with this one.
    int64 %n#1 = binop int64 %n - int64 1

    ; Instructions that have a "unit" type may omit the "unit %name =" prefix
    ; that other instructions have. Unique names for these instructions are
    ; generated automatically. You can still give them explicit names though.
    branch @fac.loop_cond(int64 %n#1, int64 %p#1)
  }

  block %exit(int64 %p) {
    return int64 %p
  }
}

function unit @main {
  block %entry {
    ; scall invokes a function statically. There are several other kinds of call
    ; instructions: new invokes a constructor, pcall calls a function pointer,
    ; vcall calls a method.
    int64 %p = scall @fac(int64 5)

    ; Tungsten does not support any implicit casting. All operations are explicit.
    ; itruncate is used to cast larger integers types to smaller ones.
    int32 %p#1 = itruncate int64 %p

    ; intrinsic is another function call instruction. It is used to call built-in
    ; functions like exit, which terminates the program with an error code. These
    ; functions are provided by a runtime library which must be linked into the
    ; final program. For the LLVM backend, the runtime is in llvm/runtime.
    intrinsic exit(int32 %p#1)
  }
}
