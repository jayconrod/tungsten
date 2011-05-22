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
