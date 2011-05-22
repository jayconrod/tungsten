name: @tungsten
type: intermediate
version: v0.3
is64bit: true
safe: true
class @tungsten.Object

annotation @tungsten.Location(string %filename,
                              int32 %beginLine,
                              int32 %beginColumn,
                              int32 %endLine,
                              int32 %endColumn)
annotation @tungsten.Abstract
annotation @tungsten.Final

global int32 @tungsten.STDIN = int32 0
global int32 @tungsten.STDOUT = int32 1
global int32 @tungsten.STDERR = int32 2

; The following constants apply to Linux only. May need to define
; different constants for other Posix systems.
global int32 @tungsten.O_RDONLY = int32 0
global int32 @tungsten.O_WRONLY = int32 1
global int32 @tungsten.O_RDWR = int32 2
global int32 @tungsten.O_CREAT = int32 64
global int32 @tungsten.O_EXCL = int32 128
global int32 @tungsten.O_TRUNC = int32 512
global int32 @tungsten.O_APPEND = int32 1024
