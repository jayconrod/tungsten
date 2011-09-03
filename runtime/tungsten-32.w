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
type: library
version: v0.5
is64bit: false
safe: true

class @tungsten.Object {
  constructors { %ctor }
}
function unit @tungsten.Object.ctor(class @tungsten.Object %this) {
  block %entry {
    return ()
  }
}

class @tungsten.Exception <: class @tungsten.Object {
  constructors { %ctor }
}
function unit @tungsten.Exception.ctor(class @tungsten.Exception %this) {
  block %entry {
    class @tungsten.Object %super = upcast class @tungsten.Exception @tungsten.Exception.ctor.this
    scall @tungsten.Object.ctor(class @tungsten.Object %super)
    return ()
  }
}

interface @tungsten.RuntimeException <: class @tungsten.Exception

class @tungsten.NullPointerException <: class @tungsten.Exception {
  interface @tungsten.RuntimeException
  constructors { %ctor }
}
function unit @tungsten.NullPointerException.ctor(class @tungsten.NullPointerException %this) {
  block %entry {
    class @tungsten.Exception %super = upcast class @tungsten.NullPointerException @tungsten.NullPointerException.ctor.this
    scall @tungsten.Exception.ctor(class @tungsten.Exception %super)
    return ()
  }
}
