//// BitArrays are a sequence of binary data of any length.

import gleam/string

/// Converts a UTF-8 `String` type into a `BitArray`.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_from_string")
pub fn from_string(x: String) -> BitArray

/// Returns an integer which is the number of bytes in the bit array.
///
@external(erlang, "erlang", "byte_size")
@external(javascript, "../gleam_stdlib.mjs", "length")
pub fn byte_size(x: BitArray) -> Int

/// Creates a new bit array by joining two bit arrays.
///
/// ## Examples
///
/// ```gleam
/// > append(to: from_string("butter"), suffix: from_string("fly"))
/// from_string("butterfly")
/// ```
///
pub fn append(to first: BitArray, suffix second: BitArray) -> BitArray {
  concat([first, second])
}

/// Extracts a sub-section of a bit array.
///
/// The slice will start at given position and continue up to specified
/// length.
/// A negative length can be used to extract bytes at the end of a bit array.
///
/// This function runs in constant time.
///
@external(erlang, "gleam_stdlib", "bit_array_slice")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_slice")
pub fn slice(
  from string: BitArray,
  at position: Int,
  take length: Int,
) -> Result(BitArray, Nil)

/// Tests to see whether a bit array is valid UTF-8.
///
pub fn is_utf8(bits: BitArray) -> Bool {
  do_is_utf8(bits)
}

@target(erlang)
fn do_is_utf8(bits: BitArray) -> Bool {
  case bits {
    <<>> -> True
    <<_:utf8, rest:bytes>> -> do_is_utf8(rest)
    _ -> False
  }
}

@target(javascript)
fn do_is_utf8(bits: BitArray) -> Bool {
  case to_string(bits) {
    Ok(_) -> True
    _ -> False
  }
}

/// Converts a bit array to a string.
///
/// Returns an error if the bit array is invalid UTF-8 data.
///
pub fn to_string(bits: BitArray) -> Result(String, Nil) {
  do_to_string(bits)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "identity")
fn unsafe_to_string(a: BitArray) -> String

@target(erlang)
fn do_to_string(bits: BitArray) -> Result(String, Nil) {
  case is_utf8(bits) {
    True -> Ok(unsafe_to_string(bits))
    False -> Error(Nil)
  }
}

@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "bit_array_to_string")
fn do_to_string(a: BitArray) -> Result(String, Nil)

/// Creates a new bit array by joining multiple binaries.
///
/// ## Examples
///
/// ```gleam
/// > concat([from_string("butter"), from_string("fly")])
/// from_string("butterfly")
/// ```
///
@external(erlang, "gleam_stdlib", "bit_array_concat")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_concat")
pub fn concat(bit_arrays: List(BitArray)) -> BitArray

/// Encodes a BitArray into a base 64 encoded string.
///
pub fn base64_encode(input: BitArray, padding: Bool) -> String {
  let encoded = encode64(input)
  case padding {
    True -> encoded
    False -> string.replace(encoded, "=", "")
  }
}

@external(erlang, "base64", "encode")
@external(javascript, "../gleam_stdlib.mjs", "encode64")
fn encode64(a: BitArray) -> String

/// Decodes a base 64 encoded string into a `BitArray`.
///
pub fn base64_decode(encoded: String) -> Result(BitArray, Nil) {
  let padded = case byte_size(from_string(encoded)) % 4 {
    0 -> encoded
    n -> string.append(encoded, string.repeat("=", 4 - n))
  }
  decode64(padded)
}

@external(erlang, "gleam_stdlib", "base_decode64")
@external(javascript, "../gleam_stdlib.mjs", "decode64")
fn decode64(a: String) -> Result(BitArray, Nil)

/// Encodes a `BitArray` into a base 64 encoded string with URL and filename safe alphabet.
///
pub fn base64_url_encode(input: BitArray, padding: Bool) -> String {
  base64_encode(input, padding)
  |> string.replace("+", "-")
  |> string.replace("/", "_")
}

/// Decodes a base 64 encoded string with URL and filename safe alphabet into a `BitArray`.
///
pub fn base64_url_decode(encoded: String) -> Result(BitArray, Nil) {
  encoded
  |> string.replace("-", "+")
  |> string.replace("_", "/")
  |> base64_decode()
}

@external(erlang, "binary", "encode_hex")
@external(javascript, "../gleam_stdlib.mjs", "base16_encode")
pub fn base16_encode(input: BitArray) -> String

@external(erlang, "gleam_stdlib", "base16_decode")
@external(javascript, "../gleam_stdlib.mjs", "base16_decode")
pub fn base16_decode(input: String) -> Result(BitArray, Nil)
