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
  ; bit  0: always 1 (is class)
  ; all other bits reserved
  field int32 %flags

  field struct @tungsten.array %name
  field struct @tungsten.array %type_parameters

  field struct @tungsten.class_info*? %superclass  

  ; Number of supertypes including classes and interface, not including this class
  field int64 %supertype_count

  ; Pointer to an array of pointers to class/interface info for each supertype. 
  ; Note that the first field of class/interface info is both flags. The low bit
  ; of the flags field can be checked to determine whether the target is a class
  ; or interface.
  field struct @tungsten.class_info**? %supertype_info 

  ; Pointer to an array of pointers to arrays of instructions for building 
  ; supertype type arguments out of an instance's type arguments. This will be
  ; null if no supertypes have reified type arguments. Each entry will be null
  ; if the corresponding supertype (in %supertype_info) has no reified type
  ; arguments. Otherwise, each entry points to an array of instructions. Each
  ; instruction can be one of:
  ;   - null (no bits set): Nothing type
  ;   - pointer (low bit not set): pointer to class/interface info
  ;   - index (low bit set, shift to access): index of instance type argument
  ;   - end (all bits set): no more instructions
  ; See @tungsten.instanceof function for how this is used.
  field int8*?*?*? %supertype_instructions
}

struct @tungsten.interface_info {
  ; bit 0: always 0 (is interface)
  ; all other bits reserved
  field int32 %flags

  field struct @tungsten.array %name
  field struct @tungsten.array %type_parameters
}

struct @tungsten.type_parameter_info {
  ; bit 0: 0 if invariant, 1 if co- or contravariant
  ; bit 1: 0 if covariant, 1 if contravariant (only meaningful if bit 0 is set)
  field int32 %flags
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

function boolean @tungsten.instanceof(struct @tungsten.Object.data$* %object,
                                      struct @tungsten.class_info* %isa_class,
                                      struct @tungsten.class_info*? %isa_type_args)
