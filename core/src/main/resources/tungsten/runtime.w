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

; This file contains definitions which should be available to programs
; compiled to Tungsten. The assembler will automatically add these 
; definitions to code it assembles (unless it is instructed not to).
; Definitions here are considered "special", and the compiler may 
; expect them to be present.
;
; No code here is 64- or 32-bit specific, so the 64-bit flag can be
; changed as needed.
;
; The implementations of functions declared here can be found in 
; core/runtime/tungsten-{32,64}.w.

name: @tungsten
type: intermediate
version: v0.5
is64bit: true
safe: true

class @tungsten.Object {
  constructors { %ctor }
}
function unit @tungsten.Object.ctor(class @tungsten.Object %this)

class @tungsten.Exception <: class @tungsten.Object {
  constructors { %ctor }
}
function unit @tungsten.Exception.ctor(class @tungsten.Exception %this)

interface @tungsten.RuntimeException <: class @tungsten.Exception

class @tungsten.NullPointerException <: class @tungsten.Exception {
  interface @tungsten.RuntimeException
  constructors { %ctor }
}
function unit @tungsten.NullPointerException.ctor(class @tungsten.NullPointerException %this)

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

; The following constants apply to Linux only. May need to define
; different constants for other Posix systems.
global int32 @tungsten.O_RDONLY = int32 0
global int32 @tungsten.O_WRONLY = int32 1
global int32 @tungsten.O_RDWR = int32 2
global int32 @tungsten.O_CREAT = int32 64
global int32 @tungsten.O_EXCL = int32 128
global int32 @tungsten.O_TRUNC = int32 512
global int32 @tungsten.O_APPEND = int32 1024
