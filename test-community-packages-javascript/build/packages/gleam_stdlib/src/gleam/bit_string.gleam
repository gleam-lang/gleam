//// Working with raw bit string data.
//// The `BitString` type should be used instead of a String type when not utf8
//// encoded.

/// Converts a UTF-8 `String` type into a raw `BitString` type.
///
pub fn from_string(x: String) -> BitString {
  do_from_string(x)
}

if erlang {
  external fn do_from_string(String) -> BitString =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_from_string(String) -> BitString =
    "../gleam_stdlib.mjs" "bit_string_from_string"
}

/// Returns an integer which is the number of bytes in the bit string.
///
pub fn byte_size(x: BitString) -> Int {
  do_byte_size(x)
}

if erlang {
  external fn do_byte_size(BitString) -> Int =
    "erlang" "byte_size"
}

if javascript {
  external fn do_byte_size(BitString) -> Int =
    "../gleam_stdlib.mjs" "length"
}

/// Creates a new bit string by joining two binaries.
///
/// ## Examples
///
/// ```gleam
/// > append(to: from_string("butter"), suffix: from_string("fly"))
/// from_string("butterfly")
/// ```
///
pub fn append(to first: BitString, suffix second: BitString) -> BitString {
  concat([first, second])
}

/// Extracts a sub-section of a bit string.
///
/// The slice will start at given position and continue up to specified
/// length.
/// A negative length can be used to extract bytes at the end of a bit string.
///
/// This function runs in constant time.
///
pub fn slice(
  from string: BitString,
  at position: Int,
  take length: Int,
) -> Result(BitString, Nil) {
  do_slice(string, position, length)
}

if erlang {
  external fn do_slice(
    string: BitString,
    position: Int,
    length: Int,
  ) -> Result(BitString, Nil) =
    "gleam_stdlib" "bit_string_slice"
}

if javascript {
  external fn do_slice(
    string: BitString,
    position: Int,
    length: Int,
  ) -> Result(BitString, Nil) =
    "../gleam_stdlib.mjs" "bit_string_slice"
}

/// Tests to see whether a bit string is valid UTF-8.
///
pub fn is_utf8(bits: BitString) -> Bool {
  do_is_utf8(bits)
}

if erlang {
  fn do_is_utf8(bits: BitString) -> Bool {
    case bits {
      <<>> -> True
      <<_:utf8, rest:binary>> -> do_is_utf8(rest)
      _ -> False
    }
  }
}

if javascript {
  fn do_is_utf8(bits: BitString) -> Bool {
    case to_string(bits) {
      Ok(_) -> True
      _ -> False
    }
  }
}

/// Converts a bit string to a string.
///
/// Returns an error if the bit string is invalid UTF-8 data.
///
pub fn to_string(bits: BitString) -> Result(String, Nil) {
  do_to_string(bits)
}

if erlang {
  external fn unsafe_to_string(BitString) -> String =
    "gleam_stdlib" "identity"

  fn do_to_string(bits: BitString) -> Result(String, Nil) {
    case is_utf8(bits) {
      True -> Ok(unsafe_to_string(bits))
      False -> Error(Nil)
    }
  }
}

if javascript {
  external fn do_to_string(BitString) -> Result(String, Nil) =
    "../gleam_stdlib.mjs" "bit_string_to_string"
}

/// Creates a new bit string by joining multiple binaries.
///
/// ## Examples
///
/// ```gleam
/// > concat([from_string("butter"), from_string("fly")])
/// from_string("butterfly")
/// ```
///
pub fn concat(bit_strings: List(BitString)) -> BitString {
  do_concat(bit_strings)
}

if erlang {
  external fn do_concat(List(BitString)) -> BitString =
    "gleam_stdlib" "bit_string_concat"
}

if javascript {
  external fn do_concat(List(BitString)) -> BitString =
    "../gleam_stdlib.mjs" "bit_string_concat"
}
