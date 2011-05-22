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

