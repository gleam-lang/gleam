//// This module has been deprecated in favour of `gleam/bytes_builder`.

import gleam/bytes_builder
import gleam/string_builder.{type StringBuilder}

pub type BitBuilder =
  bytes_builder.BytesBuilder

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn new() -> BitBuilder {
  bytes_builder.new()
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn prepend(to: BitBuilder, prefix: BitArray) -> BitBuilder {
  bytes_builder.prepend(to, prefix)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn append(to: BitBuilder, suffix: BitArray) -> BitBuilder {
  bytes_builder.append(to, suffix)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn prepend_builder(to: BitBuilder, prefix: BitBuilder) -> BitBuilder {
  bytes_builder.prepend_builder(to, prefix)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn append_builder(
  to first: BitBuilder,
  suffix second: BitBuilder,
) -> BitBuilder {
  bytes_builder.append_builder(first, second)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn prepend_string(to: BitBuilder, prefix: String) -> BitBuilder {
  bytes_builder.prepend_string(to, prefix)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn append_string(to: BitBuilder, suffix: String) -> BitBuilder {
  bytes_builder.append_string(to, suffix)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn concat(builders: List(BitBuilder)) -> BitBuilder {
  bytes_builder.concat(builders)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn concat_bit_strings(bits: List(BitArray)) -> BitBuilder {
  bytes_builder.concat_bit_arrays(bits)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn from_string(string: String) -> BitBuilder {
  bytes_builder.from_string(string)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn from_string_builder(builder: StringBuilder) -> BitBuilder {
  bytes_builder.from_string_builder(builder)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn from_bit_string(bits: BitArray) -> BitBuilder {
  bytes_builder.from_bit_array(bits)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn to_bit_string(builder: BitBuilder) -> BitArray {
  bytes_builder.to_bit_array(builder)
}

@deprecated("Please use the `gleam/bytes_builder` module instead.")
pub fn byte_size(builder: BitBuilder) -> Int {
  bytes_builder.byte_size(builder)
}
