/* Copyright 2009-2011 Jay Conrod
 *
 * This file is part of Tungsten.
 *
 * Tungsten is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation, either version 2 of 
 * the License, or (at your option) any later version.
 *
 * Tungsten is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public 
 * License along with Tungsten.  If not, see 
 * <http://www.gnu.org/licenses/>.
 */

/* This file contains definitions related to the Tungsten object system. In LLVM, a Tungsten
 * object is represented by a block of memory containing a vtable pointer, a number of fields
 * (stored in the same order as declared in the class, aligned appropriately), and a number of
 * pointers used to represent reified type arguments. Each word in the type argument list
 * is either a pointer to a class_info, an interface_info, or null (for the nothing type). 
 * If the type argument has more type arguments, they are stored immediately afterward. For
 * example, if an object has the class A[B[C, D], E[F, G]], the type argument words would be
 * B, C, D, E, F, G.
 */

#ifndef objects_h
#define objects_h

#include <stdint.h>

typedef struct w_array {
  int8_t* data;
  intptr_t size;
} w_array;

const uint32_t CLASS_INFO_KIND_MASK = 0x1;
const uint32_t CLASS_INFO_CLASS_FLAG = 0x1;
const uint32_t CLASS_INFO_INTERFACE_FLAG = 0x0;

typedef struct w_class_info {
  uint32_t flags;
  w_array name;
  w_array type_parameters;
  const struct w_class_info* superclass;
  intptr_t supertype_count;
  const struct w_class_info* const* supertype_info;
  const int8_t* const* const* supertype_instructions;
  intptr_t instance_size;
} w_class_info;

typedef struct w_interface_info {
  uint32_t flags;
  w_array name;
  w_array type_parameters;
  const struct w_class_info* superclass;
  intptr_t supertype_count;
  const struct w_class_info* const* supertype_info;
  const int8_t* const* const* supertype_instructions;
} w_interface_info;

const uint32_t TYPE_PARAMETER_INFO_VARIANCE_MASK = 0x3;
const uint32_t TYPE_PARAMETER_INFO_INVARIANT_FLAG = 0x0;
const uint32_t TYPE_PARAMETER_INFO_COVARIANT_FLAG = 0x1;
const uint32_t TYPE_PARAMETER_INFO_CONTRAVARIANT_FLAG = 0x2;

typedef struct w_type_parameter_info {
  uint32_t flags;
} w_type_parameter_info;

typedef struct w_vtable {
  const w_class_info* info;
} w_vtable;
  
typedef struct w_object {
  const w_vtable* vtable;
  const w_class_info const* type_args[0];
} w_object;

#endif
