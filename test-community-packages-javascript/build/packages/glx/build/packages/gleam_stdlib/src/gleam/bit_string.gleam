//// This module has been deprecated. Please use the `gleam/bit_array` module
//// instead.

import gleam/bit_array

@deprecated("Please use the `gleam/bit_array` module instead.")
pub fn from_string(x: String) -> BitArray {
  bit_array.from_string(x)
}

@deprecated("Please use the `gleam/bit_array` module instead.")
pub fn byte_size(x: BitArray) -> Int {
  bit_array.byte_size(x)
}

@deprecated("Please use the `gleam/bit_array` module instead.")
pub fn append(to first: BitArray, suffix second: BitArray) -> BitArray {
  bit_array.append(first, second)
}

@deprecated("Please use the `gleam/bit_array` module instead.")
pub fn slice(
  from string: BitArray,
  at position: Int,
  take length: Int,
) -> Result(BitArray, Nil) {
  bit_array.slice(string, position, length)
}

@deprecated("Please use the `gleam/bit_array` module instead.")
pub fn is_utf8(bits: BitArray) -> Bool {
  bit_array.is_utf8(bits)
}

@deprecated("Please use the `gleam/bit_array` module instead.")
pub fn to_string(bits: BitArray) -> Result(String, Nil) {
  bit_array.to_string(bits)
}

@deprecated("Please use the `gleam/bit_array` module instead.")
pub fn concat(bit_strings: List(BitArray)) -> BitArray {
  bit_array.concat(bit_strings)
}
