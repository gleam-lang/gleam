//// `BytesBuilder` is a type used for efficiently building binary content to be
//// written to a file or a socket. Internally it is represented as tree so to
//// append or prepend to a bytes builder is a constant time operation that
//// allocates a new node in the tree without copying any of the content. When
//// writing to an output stream the tree is traversed and the content is sent
//// directly rather than copying it into a single buffer beforehand.
////
//// If we append one bit array to another the bit arrays must be copied to a
//// new location in memory so that they can sit together. This behaviour
//// enables efficient reading of the data but copying can be expensive,
//// especially if we want to join many bit arrays together.
////
//// BytesBuilder is different in that it can be joined together in constant
//// time using minimal memory, and then can be efficiently converted to a
//// bit array using the `to_bit_array` function.
////
//// Byte builders are always byte aligned, so that a number of bits that is not
//// divisible by 8 will be padded with 0s.
////
//// On Erlang this type is compatible with Erlang's iolists.

// TODO: pad bit arrays to byte boundaries when adding to a builder.
import gleam/bit_array
import gleam/list
import gleam/string_builder.{type StringBuilder}

pub opaque type BytesBuilder {
  Bytes(BitArray)
  Text(StringBuilder)
  Many(List(BytesBuilder))
}

/// Create an empty `BytesBuilder`. Useful as the start of a pipe chaining many
/// builders together.
///
pub fn new() -> BytesBuilder {
  concat([])
}

/// Prepends a bit array to the start of a builder.
///
/// Runs in constant time.
///
pub fn prepend(to second: BytesBuilder, prefix first: BitArray) -> BytesBuilder {
  append_builder(from_bit_array(first), second)
}

/// Appends a bit array to the end of a builder.
///
/// Runs in constant time.
///
pub fn append(to first: BytesBuilder, suffix second: BitArray) -> BytesBuilder {
  append_builder(first, from_bit_array(second))
}

/// Prepends a builder onto the start of another.
///
/// Runs in constant time.
///
pub fn prepend_builder(
  to second: BytesBuilder,
  prefix first: BytesBuilder,
) -> BytesBuilder {
  append_builder(first, second)
}

/// Appends a builder onto the end of another.
///
/// Runs in constant time.
///
@external(erlang, "gleam_stdlib", "iodata_append")
pub fn append_builder(
  to first: BytesBuilder,
  suffix second: BytesBuilder,
) -> BytesBuilder {
  case second {
    Many(builders) -> Many([first, ..builders])
    _ -> Many([first, second])
  }
}

/// Prepends a string onto the start of a builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
pub fn prepend_string(
  to second: BytesBuilder,
  prefix first: String,
) -> BytesBuilder {
  append_builder(from_string(first), second)
}

/// Appends a string onto the end of a builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
pub fn append_string(
  to first: BytesBuilder,
  suffix second: String,
) -> BytesBuilder {
  append_builder(first, from_string(second))
}

/// Joins a list of builders into a single builder.
///
/// Runs in constant time.
///
@external(erlang, "gleam_stdlib", "identity")
pub fn concat(builders: List(BytesBuilder)) -> BytesBuilder {
  Many(builders)
}

/// Joins a list of bit arrays into a single builder.
///
/// Runs in constant time.
///
@external(erlang, "gleam_stdlib", "identity")
pub fn concat_bit_arrays(bits: List(BitArray)) -> BytesBuilder {
  bits
  |> list.map(fn(b) { from_bit_array(b) })
  |> concat()
}

/// Creates a new builder from a string.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_string(string: String) -> BytesBuilder {
  Text(string_builder.from_string(string))
}

/// Creates a new builder from a string builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_string_builder(builder: StringBuilder) -> BytesBuilder {
  Text(builder)
}

/// Creates a new builder from a bit array.
///
/// Runs in constant time.
///
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_bit_array(bits: BitArray) -> BytesBuilder {
  Bytes(bits)
}

/// Turns an builder into a bit array.
///
/// Runs in linear time.
///
/// When running on Erlang this function is implemented natively by the
/// virtual machine and is highly optimised.
///
@external(erlang, "erlang", "list_to_bitstring")
pub fn to_bit_array(builder: BytesBuilder) -> BitArray {
  [[builder]]
  |> to_list([])
  |> list.reverse
  |> bit_array.concat
}

fn to_list(
  stack: List(List(BytesBuilder)),
  acc: List(BitArray),
) -> List(BitArray) {
  case stack {
    [] -> acc

    [[], ..remaining_stack] -> to_list(remaining_stack, acc)

    [[Bytes(bits), ..rest], ..remaining_stack] ->
      to_list([rest, ..remaining_stack], [bits, ..acc])

    [[Text(builder), ..rest], ..remaining_stack] -> {
      let bits = bit_array.from_string(string_builder.to_string(builder))
      to_list([rest, ..remaining_stack], [bits, ..acc])
    }

    [[Many(builders), ..rest], ..remaining_stack] ->
      to_list([builders, rest, ..remaining_stack], acc)
  }
}

/// Returns the size of the builder's content in bytes.
///
/// Runs in linear time.
///
@external(erlang, "erlang", "iolist_size")
pub fn byte_size(builder: BytesBuilder) -> Int {
  [[builder]]
  |> to_list([])
  |> list.fold(0, fn(acc, builder) { bit_array.byte_size(builder) + acc })
}
