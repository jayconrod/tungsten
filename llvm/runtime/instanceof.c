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

/* This file implements the instanceof operator, which is surprisingly complex due to reified
 * type arguments.
 *
 * The main function is subtypeof, which implements the subtype relation for class and 
 * interface types. Other functions are used to implement this. wrt_instanceof just sets up
 * the call to subtypeof.
 */

#include "objects.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

static bool subtypeof(const w_class_info* s_class, 
                      const w_class_info* const* s_type_args,
                      const intptr_t* s_type_args_index,
                      const w_class_info* t_class, 
                      const w_class_info* const* t_type_args,
                      const intptr_t* t_type_args_index);

static bool equals(const w_class_info* s_class, 
                   const w_class_info* const* s_type_args,
                   const intptr_t* s_type_args_index,
                   const w_class_info* t_class, 
                   const w_class_info* const* t_type_args,
                   const intptr_t* t_type_args_index);

static inline intptr_t find_supertype(const w_class_info* s_class, const w_class_info* t_class);

static intptr_t find_type_args_size(intptr_t type_parameter_count,
                                    const w_class_info* const* type_args);

static void index_type_args(intptr_t type_parameter_count,
                            const w_class_info* const* type_args,
                            intptr_t* type_arg_index,
                            intptr_t* offset);

static inline intptr_t find_super_type_args_size(const int8_t* const* instructions, 
                                                 intptr_t* type_arg_offsets);

static inline void execute_supertype_instructions(const int8_t* const* instructions,
                                                  const w_class_info* const* type_args,
                                                  intptr_t* type_arg_offsets,
                                                  const w_class_info** super_type_args);

/* This function is the top level implementation of the instanceof operator. This is what
 * will actually get called by the program.
 *  object: a pointer to the object being tested
 *  isa_class: a pointer to the info of the class or interface which we are asking about
 *  isa_type_args: type arguments for the class or interface, stored in the same depth-first
 *    format as the object.
 */
bool wrt_instanceof(const w_object* object, 
                    const w_class_info* isa_class,
                    const w_class_info* const* isa_type_args)
{
  const w_class_info* obj_class = object->vtable->info;
  intptr_t obj_type_args_offset = obj_class->instance_size / sizeof(w_class_info*) - 1;
  const w_class_info* const* obj_type_args = object->type_args + obj_type_args_offset;
  intptr_t obj_type_args_size = 
    find_type_args_size(obj_class->type_parameters.size, obj_type_args);
  intptr_t obj_type_args_index[obj_type_args_size];
  index_type_args(obj_class->type_parameters.size, obj_type_args, obj_type_args_index, NULL);

  intptr_t isa_type_args_size = 
    find_type_args_size(isa_class->type_parameters.size, isa_type_args);
  intptr_t isa_type_args_index[isa_type_args_size];
  index_type_args(isa_class->type_parameters.size, isa_type_args, isa_type_args_index, NULL);

  return subtypeof(obj_class, obj_type_args, obj_type_args_index,
                   isa_class, isa_type_args, isa_type_args_index);
}

/* Test whether one object type S is a subtype of another T.
 * {s,t}_class: the class or interface of the types
 * {s,t}_type_args: a list of type argument words for S and T, stored in depth-first format
 * {s,t}_type_args_index: an index used to navigate the type argument words. See index_type_args.
 */
static bool subtypeof(const w_class_info* s_class, 
                      const w_class_info* const* s_type_args,
                      const intptr_t* s_type_args_index,
                      const w_class_info* t_class, 
                      const w_class_info* const* t_type_args,
                      const intptr_t* t_type_args_index)
{
  if (s_class == NULL) {
    // Nothing is a subtype of everything
    return true;
  } else if (t_class == NULL) {
    // There is no subtype of nothing
    return false;
  } else if (s_class != t_class) {
    // If S and T are of different classes, we can't compare them directly. We need to see if
    // S is a subtype of another type, S', which IS of the same class as T. Every class_info
    // and interface_info has a list of supertypes and an array of instructions for converting
    // to each supertype.

    // Determine whether we even support the other type
    intptr_t supertype_index = find_supertype(s_class, t_class);
    if (supertype_index == s_class->supertype_count)
      return false;

    // If we do support the other type, we need to convert S to that type. If there are no
    // instructions for doing so, T must have no reified type parameters, so we can stop.
    if (s_class->supertype_instructions == NULL)
      return true;
    const int8_t* const* supertype_instructions = 
      s_class->supertype_instructions[supertype_index];
    if (supertype_instructions == NULL)
      return true;

    // Do the actual conversion. We create a small array of offsets to the end of each 
    // type argument so we can easily move whole arguments. We create a new array of type
    // arguments of the appropriate size and execute the instructions.
    intptr_t s_type_arg_count = s_class->type_parameters.size;
    intptr_t s_type_arg_offsets[s_type_arg_count];
    intptr_t current_offset = 0;
    for (intptr_t i = 0; i < s_type_arg_count; i++) {
      current_offset += s_type_args_index[current_offset];
      s_type_arg_offsets[i] = current_offset;
    }
    intptr_t super_type_args_size = find_super_type_args_size(supertype_instructions,
                                                              s_type_arg_offsets);
    const w_class_info* super_type_args[super_type_args_size];
    execute_supertype_instructions(supertype_instructions,
                                   s_type_args,
                                   s_type_arg_offsets,
                                   super_type_args);

    // We need to create a new index for the converted type arguments. This can probably
    // be optimized to be done at the same time.
    intptr_t super_type_args_index[super_type_args_size];
    index_type_args(t_class->type_parameters.size, super_type_args, super_type_args_index, NULL);

    s_class = t_class;
    s_type_args = super_type_args;
    s_type_args_index = super_type_args_index;
  }

  // At this point, S and T are of the same class and will have the same number of type 
  // arguments. We just need to compare each pair of type arguments according to the variance
  // of the corresponding parameter.
  const w_type_parameter_info* tp_info = 
    (const w_type_parameter_info*) t_class->type_parameters.data;
  intptr_t next_s = 0, next_t = 0;
  for (intptr_t i = 0; i < t_class->type_parameters.size; ++i) {
    const w_class_info* tp_s_class = s_type_args[next_s];
    const w_class_info* const* tp_s_type_args = s_type_args + next_s + 1;
    const intptr_t* tp_s_type_args_index = s_type_args_index + next_s + 1;
    next_s += s_type_args_index[next_s];

    const w_class_info* tp_t_class = t_type_args[next_t];
    const w_class_info* const* tp_t_type_args = t_type_args + next_t + 1;
    const intptr_t* tp_t_type_args_index = t_type_args_index + next_t + 1;
    next_t += t_type_args_index[next_t];

    uint32_t variance = tp_info[i].flags & TYPE_PARAMETER_INFO_VARIANCE_MASK;
    if (variance == TYPE_PARAMETER_INFO_INVARIANT_FLAG) {
      if(!equals(tp_s_class, tp_s_type_args, tp_s_type_args_index,
                 tp_t_class, tp_t_type_args, tp_t_type_args_index))
        return false;
    } else if (variance == TYPE_PARAMETER_INFO_COVARIANT_FLAG) {
      if (!subtypeof(tp_s_class, tp_s_type_args, tp_s_type_args_index,
                     tp_t_class, tp_t_type_args, tp_t_type_args_index))
        return false;
    } else { // variance == TYPE_PARAMETER_INFO_CONTRAVARIANT_FLAG
      if (!subtypeof(tp_t_class, tp_t_type_args, tp_t_type_args_index,
                     tp_s_class, tp_s_type_args, tp_s_type_args_index))
        return false;
    }
  }
  return true;
}

/* Check whether two types are exactly the same. This is used to compare invariant 
 * type parameters. The arguments are in the same format as subtypeof.
 */
static bool equals(const w_class_info* s_class, 
                   const w_class_info* const* s_type_args,
                   const intptr_t* s_type_args_index,
                   const w_class_info* t_class, 
                   const w_class_info* const* t_type_args,
                   const intptr_t* t_type_args_index)
{
  if (s_class != t_class)
    return false;
  if (s_class == NULL)
    return true;

  intptr_t next_s = 0, next_t = 0;
  for (intptr_t i = 0; i < s_class->type_parameters.size; ++i) {
    const w_class_info* tp_s_class = s_type_args[next_s];
    const w_class_info* const* tp_s_type_args = s_type_args + next_s + 1;
    const intptr_t* tp_s_type_args_index = s_type_args_index + next_s + 1;
    next_s += s_type_args_index[next_s];

    const w_class_info* tp_t_class = t_type_args[next_t];
    const w_class_info* const* tp_t_type_args = t_type_args + next_t + 1;
    const intptr_t* tp_t_type_args_index = t_type_args_index + next_t + 1;
    next_t += t_type_args_index[next_t];

    if (!equals(tp_s_class, tp_s_type_args, tp_s_type_args_index,
                tp_t_class, tp_t_type_args, tp_t_type_args_index))
      return false;
  }
  return true;
}

/* Every class_info and interface_info has an array of pointers to info records for supertypes.
 * This includes not just direct supertypes but supertypes of direct supertypes, etc. This 
 * function returns the index of the desired info within this array or supertype_count if not
 * found. The index can be used to locate other information associated with the type, such as
 * instructions for converting type arguments.
 * s_class: the info to search
 * t_class: the supertype info to search for
 */
static inline intptr_t find_supertype(const w_class_info* s_class, const w_class_info* t_class) {
  intptr_t supertype_index;
  intptr_t supertype_count = s_class->supertype_count;
  for (supertype_index = 0; supertype_index < supertype_count; ++supertype_index) {
    if (s_class->supertype_info[supertype_index] == t_class)
      break;
  }
  return supertype_index;
}

/* Determines the size in words of the given type argument array. */
static intptr_t find_type_args_size(intptr_t type_parameter_count,
                                    const w_class_info* const* type_args)
{
  intptr_t size = 0;
  for (intptr_t i = 0; i < type_parameter_count; ++i) {
    const w_class_info* type = type_args[size++];
    if (type != NULL)
      size += find_type_args_size(type->type_parameters.size, type_args + size);
  }
  return size;
}

/* Creates an index of the given type argument array, which is used for quick navigation.
 * The index is an array of intptr_t with the same number of elements as the type argument
 * array. Each element in the index is the size in words of the type argument starting at 
 * that position. For example, for the type arguments [B[C, D], E[F, G]], the index would be
 * [3, 1, 1, 3, 1, 1].
 */
static void index_type_args(intptr_t type_parameter_count,
                            const w_class_info* const* type_args,
                            intptr_t* type_arg_index,
                            intptr_t* offset)
{
  intptr_t default_offset = 0;
  if (offset == NULL)
    offset = &default_offset;

  for (intptr_t i = 0; i < type_parameter_count; ++i) {
    intptr_t current_offset = (*offset)++;
    const w_class_info* type = type_args[current_offset];
    if (type != NULL)
      index_type_args(type->type_parameters.size, type_args, type_arg_index, offset);
    type_arg_index[current_offset] = *offset - current_offset;
  }
}

/* Determines the size of the type arguments array for a supertype using the supertype 
 * instructions. This must be called to allocate the array before actually executing the
 * instructions.
 */
static inline intptr_t find_super_type_args_size(const int8_t* const* instructions, 
                                                 intptr_t* type_arg_offsets)
{
  intptr_t size = 0;
  while (true) {
    const int8_t* inst = *instructions++;
    if ((intptr_t) inst == -1) {
      break;
    } else if ((intptr_t) inst & 0x1) {
      intptr_t arg = (intptr_t) inst >> 1;
      intptr_t arg_size = arg == 0 ? 
        type_arg_offsets[0] :
        type_arg_offsets[arg] - type_arg_offsets[arg - 1];
      size += arg_size;
    } else {
      size += 1;
    }
  }
  return size;
}

/* Executes the supertype instructions stored in a class info. Instructions are used to 
 * convert the type arguments from one class/interface to a superclass/interface. Each
 * instruction may be one of the following: 
 *  -1: end of instructions
 *  0: nothing type
 *  low bit set: index of type argument to copy (shift right by one to access)
 *  any other value: a pointer to a class/interface info record
 */
static inline void execute_supertype_instructions(const int8_t* const* instructions,
                                                  const w_class_info* const* type_args,
                                                  intptr_t* type_arg_offsets,
                                                  const w_class_info** super_type_args)
{
  while (true) {
    const int8_t* inst = *instructions++;
    if ((intptr_t) inst == -1) {
      break;
    } else if ((intptr_t) inst & 0x1) {
      intptr_t arg = (intptr_t) inst >> 1;
      intptr_t arg_start = arg == 0 ? 0 : type_arg_offsets[arg - 1];
      intptr_t arg_end = type_arg_offsets[arg];
      for (int i = arg_start; i < arg_end; ++i)
        *super_type_args++ = type_args[i];
    } else {
      *super_type_args++ = (w_class_info*) inst;
    }
  }
}
