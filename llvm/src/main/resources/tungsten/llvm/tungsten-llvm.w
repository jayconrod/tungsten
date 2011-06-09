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

function int32 @tungsten.open(int8* %filename, int32 %flags)

function unit @tungsten.close(int32 %fd)
