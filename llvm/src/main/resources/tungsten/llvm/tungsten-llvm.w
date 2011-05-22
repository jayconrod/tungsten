name: @tungsten
type: intermediate
version: v0.3
is64bit: true
safe: true

annotation @tungsten.NoRuntime

struct @tungsten.string {
  field int16* %characters
  field int64 %length
}

function int8* @tungsten.malloc(int32 %size)

@tungsten.NoReturn
function unit @tungsten.exit(int32 %code)

function int64 @tungsten.read(int32 %fd, int8* %buffer, int64 %size)

function int64 @tungsten.write(int32 %fd, int8* %buffer, int64 %size)

function int32 @tungsten.open(struct @tungsten.string %filename, int32 %flags)

function unit @tungsten.close(int32 %fd)
