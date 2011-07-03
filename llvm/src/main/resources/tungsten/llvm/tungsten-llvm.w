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
version: v0.4
is64bit: true
safe: true

; Flow control annotations (for LLVM equivalence)
annotation @tungsten.NoRuntime

; TODO: remove this
struct @tungsten.string {
  field int16* %characters
  field int64 %length
}

; Generic memory allocation function
function int8* @tungsten.malloc(int32 %size)

; Intrinsic functions
@tungsten.NoReturn
function unit @tungsten.exit(int32 %code)

function int64 @tungsten.read(int32 %fd, int8* %buffer, int64 %size)

function int64 @tungsten.write(int32 %fd, int8* %buffer, int64 %size)

function int32 @tungsten.open(int8* %filename, int32 %flags)

function unit @tungsten.close(int32 %fd)

; Exception handling definitions
global int8* @_ZTIPv   ; void pointer type info

function int8* @__cxa_allocate_exception(int64 %size)

@tungsten.NoReturn
function unit @__cxa_throw(int8* %pexn, int8* %rtti1, int8* %rtti2)
