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

package tungsten

object BinaryModuleIDs {
  val MAGIC = 0x574F626A    // 'WObj' in big-endian

  val VERSION: (Byte, Byte) = (0, 5)

  val ANNOTATION_ID: Byte = 1
  val BLOCK_ID: Byte = 2
  val FIELD_ID: Byte = 3
  val FUNCTION_ID: Byte = 4
  val GLOBAL_ID: Byte = 5
  val PARAMETER_ID: Byte = 6
  val STRUCT_ID: Byte = 7
  val CLASS_ID: Byte = 8
  val INTERFACE_ID: Byte = 9
  val TYPE_PARAMETER_ID: Byte = 10

  val ADDRESS_INST_ID: Byte = 20
  val BINARY_OPERATOR_INST_ID: Byte = 21
  val BIT_CAST_INST_ID: Byte = 22
  val BRANCH_INST_ID: Byte = 23
  val CONDITIONAL_BRANCH_INST_ID: Byte = 24
  val EXTRACT_INST_ID: Byte = 25
  val INSERT_INST_ID: Byte = 26
  val INTRINSIC_CALL_INST_ID: Byte = 27
  val LOAD_INST_ID: Byte = 28
  val LOAD_ELEMENT_INST_ID: Byte = 29
  val NEW_INST_ID: Byte = 30
  val RELATIONAL_OPERATOR_INST_ID: Byte = 31
  val RETURN_INST_ID: Byte = 32
  val STORE_INST_ID: Byte = 33
  val STORE_ELEMENT_INST_ID: Byte = 34
  val STACK_ALLOCATE_INST_ID: Byte = 35
  val STACK_ALLOCATE_ARRAY_INST_ID: Byte = 36
  val STATIC_CALL_INST_ID: Byte = 37
  val UPCAST_INST_ID: Byte = 38
  val HEAP_ALLOCATE_INST_ID = 39
  val HEAP_ALLOCATE_ARRAY_INST_ID = 40
  val FLOAT_EXTEND_INST_ID = 41
  val FLOAT_TO_INTEGER_INST_ID = 42
  val FLOAT_TRUNCATE_INST_ID = 43
  val INTEGER_SIGN_EXTEND_INST_ID = 44
  val INTEGER_TO_FLOAT_INST_ID = 45
  val INTEGER_TRUNCATE_INST_ID = 46
  val INTEGER_ZERO_EXTEND_INST_ID = 47
  val POINTER_CALL_INST_ID = 48
  val VIRTUAL_CALL_INST_ID = 49
  val VIRTUAL_LOOKUP_INST_ID = 50
  val THROW_INST_ID = 51
  val UNREACHABLE_INST_ID = 52
  val CATCH_INST_ID = 53
  val NULL_CHECK_INST_ID = 54
  val INSTANCE_OF_INST_ID = 55

  val BINOP_MULTIPLY_ID: Byte = 1
  val BINOP_DIVIDE_ID: Byte = 2
  val BINOP_REMAINDER_ID: Byte = 3
  val BINOP_ADD_ID: Byte = 4
  val BINOP_SUBTRACT_ID: Byte = 5
  val BINOP_LEFT_SHIFT_ID: Byte = 6
  val BINOP_RIGHT_SHIFT_ARITHMETIC_ID: Byte = 7
  val BINOP_RIGHT_SHIFT_LOGICAL_ID: Byte = 8
  val BINOP_AND_ID: Byte = 9
  val BINOP_XOR_ID: Byte = 10
  val BINOP_OR_ID: Byte = 11

  val RELOP_LESS_THAN_ID: Byte = 20
  val RELOP_LESS_EQUAL_ID: Byte = 21
  val RELOP_GREATER_THAN_ID: Byte = 22
  val RELOP_GREATER_EQUAL_ID: Byte = 23
  val RELOP_EQUAL_ID: Byte = 24
  val RELOP_NOT_EQUAL_ID: Byte = 25

  val INTRINSIC_EXIT_ID: Byte = 1

  val UNIT_TYPE_ID = 1
  val BOOLEAN_TYPE_ID = 2
  val CHAR_TYPE_ID = 3
  val STRING_TYPE_ID = 4
  val INT_TYPE_ID = 5
  val FLOAT_TYPE_ID = 6
  val POINTER_TYPE_ID = 7
  val NULL_TYPE_ID = 8
  val ARRAY_TYPE_ID = 9
  val STRUCT_TYPE_ID = 10
  val FUNCTION_TYPE_ID = 11
  val CLASS_TYPE_ID = 12
  val INTERFACE_TYPE_ID = 13
  val VARIABLE_TYPE_ID = 14
  val VARIADIC_TYPE_ID = 15
  val NOTHING_TYPE_ID = 16

  val UNIT_VALUE_ID = 1
  val BOOLEAN_VALUE_ID = 2
  val CHAR_VALUE_ID = 3
  val STRING_VALUE_ID = 4
  val INT8_VALUE_ID = 5
  val INT16_VALUE_ID = 6
  val INT32_VALUE_ID = 7
  val INT64_VALUE_ID = 8
  val FLOAT32_VALUE_ID = 9
  val FLOAT64_VALUE_ID = 10
  val NULL_VALUE_ID = 11
  val ARRAY_VALUE_ID = 12
  val STRUCT_VALUE_ID = 13
  val DEFINED_VALUE_ID = 14
  val BIT_CAST_VALUE_ID = 15

  val COVARIANT_ID = 1
  val CONTRAVARIANT_ID = 2
  val INVARIANT_ID = 3
}
