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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file contains definitions needed to convert Tungsten code
; into lower level representations, such as LLVM. Lowered code is
; still legal Tungsten code, but it does not contain higher level
; constructs such as classes, bounded arrays, or type parameters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

name: @tungsten
type: intermediate
version: v0.5
is64bit: true

;
; Data structure definitions
;
struct @tungsten.array {
  field int8* %data
  field int64 %size
}

struct @tungsten.class_info {
  field struct @tungsten.array %name
  field struct @tungsten.class_info*? %superclass
  field struct @tungsten.array %type_parameters
}

struct @tungsten.interface_info {
  field struct @tungsten.array %name
  field struct @tungsten.array %type_parameters
}

struct @tungsten.type_parameter_info {
  field int64 %flags
  field struct @tungsten.array %name
}

struct @tungsten.itable_entry {
  field struct @tungsten.interface_info* %interface
  field int8* %ivtable
}

;
; Helper functions
;
function int8* @tungsten.load_ivtable(struct @tungsten.Object.data$* %receiver,
                                      struct @tungsten.interface_info* %interface)
{
  block %entry {
    struct @tungsten.Object.vtable_type$* %vtable = loadelement struct @tungsten.Object.data$* @tungsten.load_ivtable.receiver, int64 0, int64 0
    int8* %ivt_array = loadelement struct @tungsten.Object.vtable_type$* %vtable, int64 0, int64 1, int64 0
    struct @tungsten.itable_entry* %ivt_array#1 = bitcast int8* %ivt_array
    int64 %ivt_count = loadelement struct @tungsten.Object.vtable_type$* %vtable, int64 0, int64 1, int64 1
    int64 %i = binop int64 %ivt_count - int64 1
    boolean %cmp = relop int64 %i >= int64 0
    cond boolean %cmp ? @tungsten.load_ivtable.loop(struct @tungsten.itable_entry* %ivt_array#1,
                                                    int64 %i)
                      : @tungsten.load_ivtable.abort()
  }
  block %loop(struct @tungsten.itable_entry* %ivt_array, int64 %i) {
    struct @tungsten.interface_info* %if = loadelement struct @tungsten.itable_entry* %ivt_array, int64 %i, int64 0
    boolean %cmp = relop struct @tungsten.interface_info* %if == struct @tungsten.interface_info* @tungsten.load_ivtable.interface
    cond boolean %cmp ? @tungsten.load_ivtable.found(struct @tungsten.itable_entry* %ivt_array, int64 %i)
                      : @tungsten.load_ivtable.next(struct @tungsten.itable_entry* %ivt_array, int64 %i)
  }
  block %next(struct @tungsten.itable_entry* %ivt_array, int64 %i) {
    int64 %i#1 = binop int64 %i - int64 1
    boolean %cmp = relop int64 %i#1 >= int64 0
    cond boolean %cmp ? @tungsten.load_ivtable.loop(struct @tungsten.itable_entry* %ivt_array, int64 %i#1)
                      : @tungsten.load_ivtable.abort()
  }
  block %found(struct @tungsten.itable_entry* %ivt_array, int64 %i) {
    int8* %ivtable = loadelement struct @tungsten.itable_entry* %ivt_array, int64 %i, int64 1
    return int8* %ivtable
  }
  block %abort {
    intrinsic exit(int32 -1)
    unreachable
  }
}
