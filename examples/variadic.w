; This example demonstrates the use of variadic functions by calling the C functions
; printf and scanf. Variadic functions are supported only for C/C++ compatibility 
; since they are not safe. You can declare a function which accepts a variable number
; of arguments, but you cannot access those arguments.

; Rather than marking a function as "variadic", a variadic function has an extra
; parameter at the end with the variadic type (denoted ...); see below. The 
; variadic parameter must be the last parameter, if it is present. You can also
; use the variadic type as part of a function type. The function type for printf
; would be (int8*, ...)->int32.
function int32 @scanf(int8* %format, ... %va)
function int32 @printf(int8* %format, ... %va)

global [27 x int8] @greeting = "Hello, what is your name?\0a\00"
global [5 x int8] @request = "%64s\00"
global [22 x int8] @response = "Nice to meet you, %s\0a\00"

function unit @main {
  block %entry {
    int8* %name = stackarray int64 64

    ; Arguments are passed to variadic functions as if they were normal functions.
    ; The only difference is that the compiler won't give an error if there are
    ; too many arguments.
    int32 %v#1 = scall @printf(bitcast [27 x int8]* @greeting to int8*)
    int32 %v#2 = scall @scanf(bitcast [5 x int8]* @request to int8*, int8* %name)
    int32 %v#3 = scall @printf(bitcast [22 x int8]* @response to int8*, int8* %name)
    return ()
  }
}
