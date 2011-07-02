is64bit: true

struct @tungsten.array {
  field int8* %data
  field int64 %size
}

struct @tungsten.class_info {
  field struct @tungsten.array %name
}

struct @tungsten.interface_info {
  field struct @tungsten.array %name
}

struct @tungsten.itable_entry {
  field struct @tungsten.interface_info* %interface
  field int8* %ivtable
}
