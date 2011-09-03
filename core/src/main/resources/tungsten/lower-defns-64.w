; Copyright 2009-2011 Jay Conrod
;
; This file is part of Tungsten.
;
; Tungsten is free software: you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as 
; published by the Free Software Foundation, either version 2 of 
; the License, or (at your option) any later version.
;
; Tungsten is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public 
; License along with Tungsten.  If not, see 
; <http://www.gnu.org/licenses/>.

name: @tungsten
type: intermediate
version: v0.5
is64bit: false

; This file contains definitions needed to convert Tungsten code
; into lower level representations, such as LLVM. Lowered code is
; still legal Tungsten code, but it does not contain higher level
; constructs such as classes, bounded arrays, or type parameters.

;
; Data structure definitions
;
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
