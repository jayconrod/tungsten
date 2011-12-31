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
; This file contains LLVM specific definitions used for lowering
; Tungsten code. Lowered Tungsten code is very close to the LLVM
; representation; it may refer to C standard library functions and
; LLVM intrinsic functions. It is still legal Tungsten code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

name: @tungsten
type: intermediate
version: v0.5
is64bit: true
safe: true

; Flow control annotations (for LLVM equivalence)
annotation @tungsten.NoReturn

; TODO: remove this
struct @tungsten.string {
  field int16* %characters
  field int64 %length
}

; Terminate execution with return code
@tungsten.NoReturn
function unit @tungsten.exit(int32 %code)

; Exception handling definitions
global int8* @_ZTIPv   ; void pointer type info

function int8* @__cxa_allocate_exception(int64 %size)

@tungsten.NoReturn
function unit @__cxa_throw(int8* %pexn, int8* %rtti1, int8* %rtti2)

function int8* @__cxa_begin_catch(int8* %exn)

function unit @__cxa_end_catch

function int32 @__gxx_personality_v0(... %va)

function int8* @"llvm.eh.exception"()

function int32 @"llvm.eh.selector"(int8* %exn, int8* %personality, ... %va)
