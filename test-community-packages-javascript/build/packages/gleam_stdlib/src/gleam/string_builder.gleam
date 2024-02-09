import gleam/list

/// `StringBuilder` is a type used for efficiently building strings.
///
/// When we append one string to another the strings must be copied to a
/// new location in memory so that they can sit together. This behaviour
/// enables efficient reading of the string but copying can be expensive,
/// especially if we want to join many strings together.
///
/// `StringBuilder` is different in that it can be joined together in constant time
/// using minimal memory, and then can be efficiently converted to a string
/// using the `to_string` function.
///
/// On Erlang this type is compatible with Erlang's iodata. On JavaScript this
/// type is compatible with normal strings.
///
pub external type StringBuilder

/// Create an empty `StringBuilder`. Useful as the start of a pipe chaining many
/// builders together.
///
pub fn new() -> StringBuilder {
  do_from_strings([])
}

/// Prepends a `String` onto the start of some `StringBuilder`.
///
/// Runs in constant time.
///
pub fn prepend(
  to builder: StringBuilder,
  prefix prefix: String,
) -> StringBuilder {
  append_builder(from_string(prefix), builder)
}

/// Appends a `String` onto the end of some `StringBuilder`.
///
/// Runs in constant time.
///
pub fn append(to builder: StringBuilder, suffix second: String) -> StringBuilder {
  append_builder(builder, from_string(second))
}

/// Prepends some `StringBuilder` onto the start of another.
///
/// Runs in constant time.
///
pub fn prepend_builder(
  to builder: StringBuilder,
  prefix prefix: StringBuilder,
) -> StringBuilder {
  do_append(prefix, builder)
}

/// Appends some `StringBuilder` onto the end of another.
///
/// Runs in constant time.
///
pub fn append_builder(
  to builder: StringBuilder,
  suffix suffix: StringBuilder,
) -> StringBuilder {
  do_append(builder, suffix)
}

if erlang {
  external fn do_append(StringBuilder, StringBuilder) -> StringBuilder =
    "gleam_stdlib" "iodata_append"
}

if javascript {
  external fn do_append(StringBuilder, StringBuilder) -> StringBuilder =
    "../gleam_stdlib.mjs" "add"
}

/// Converts a list of strings into a builder.
///
/// Runs in constant time.
///
pub fn from_strings(strings: List(String)) -> StringBuilder {
  do_from_strings(strings)
}

if erlang {
  external fn do_from_strings(List(String)) -> StringBuilder =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_from_strings(List(String)) -> StringBuilder =
    "../gleam_stdlib.mjs" "join"
}

/// Joins a list of builders into a single builder.
///
/// Runs in constant time.
///
pub fn concat(builders: List(StringBuilder)) -> StringBuilder {
  do_concat(builders)
}

if erlang {
  external fn do_concat(List(StringBuilder)) -> StringBuilder =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_concat(List(StringBuilder)) -> StringBuilder =
    "../gleam_stdlib.mjs" "join"
}

/// Converts a string into a builder.
///
/// Runs in constant time.
///
pub fn from_string(string: String) -> StringBuilder {
  do_from_string(string)
}

if erlang {
  external fn do_from_string(String) -> StringBuilder =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_from_string(String) -> StringBuilder =
    "../gleam_stdlib.mjs" "identity"
}

/// Turns an `StringBuilder` into a `String`
///
/// This function is implemented natively by the virtual machine and is highly
/// optimised.
///
pub fn to_string(builder: StringBuilder) -> String {
  do_to_string(builder)
}

if erlang {
  external fn do_to_string(StringBuilder) -> String =
    "unicode" "characters_to_binary"
}

if javascript {
  external fn do_to_string(StringBuilder) -> String =
    "../gleam_stdlib.mjs" "identity"
}

/// Returns the size of the `StringBuilder` in bytes.
///
pub fn byte_size(builder: StringBuilder) -> Int {
  do_byte_size(builder)
}

if erlang {
  external fn do_byte_size(StringBuilder) -> Int =
    "erlang" "iolist_size"
}

if javascript {
  external fn do_byte_size(StringBuilder) -> Int =
    "../gleam_stdlib.mjs" "length"
}

/// Joins the given builders into a new builder separated with the given string
///
pub fn join(builders: List(StringBuilder), with sep: String) -> StringBuilder {
  builders
  |> list.intersperse(from_string(sep))
  |> concat
}

/// Converts a builder to a new builder where the contents have been
/// lowercased.
///
pub fn lowercase(builder: StringBuilder) -> StringBuilder {
  do_lowercase(builder)
}

if erlang {
  external fn do_lowercase(StringBuilder) -> StringBuilder =
    "string" "lowercase"
}

if javascript {
  external fn do_lowercase(StringBuilder) -> StringBuilder =
    "../gleam_stdlib.mjs" "lowercase"
}

/// Converts a builder to a new builder where the contents have been
/// uppercased.
///
pub fn uppercase(builder: StringBuilder) -> StringBuilder {
  do_uppercase(builder)
}

if erlang {
  external fn do_uppercase(StringBuilder) -> StringBuilder =
    "string" "uppercase"
}

if javascript {
  external fn do_uppercase(StringBuilder) -> StringBuilder =
    "../gleam_stdlib.mjs" "uppercase"
}

/// Converts a builder to a new builder with the contents reversed.
///
pub fn reverse(builder: StringBuilder) -> StringBuilder {
  do_reverse(builder)
}

if erlang {
  external fn do_reverse(StringBuilder) -> StringBuilder =
    "string" "reverse"
}

if javascript {
  fn do_reverse(builder: StringBuilder) -> StringBuilder {
    builder
    |> to_string
    |> do_to_graphemes
    |> list.reverse
    |> from_strings
  }

  external fn do_to_graphemes(string: String) -> List(String) =
    "../gleam_stdlib.mjs" "graphemes"
}

/// Splits a builder on a given pattern into a list of builders.
///
pub fn split(iodata: StringBuilder, on pattern: String) -> List(StringBuilder) {
  do_split(iodata, pattern)
}

if erlang {
  type Direction {
    All
  }

  external fn erl_split(StringBuilder, String, Direction) -> List(StringBuilder) =
    "string" "split"

  fn do_split(iodata: StringBuilder, pattern: String) -> List(StringBuilder) {
    erl_split(iodata, pattern, All)
  }
}

if javascript {
  external fn do_split(
    builder: StringBuilder,
    pattern: String,
  ) -> List(StringBuilder) =
    "../gleam_stdlib.mjs" "split"
}

/// Replaces all instances of a pattern with a given string substitute.
///
pub fn replace(
  in builder: StringBuilder,
  each pattern: String,
  with substitute: String,
) -> StringBuilder {
  do_replace(builder, pattern, substitute)
}

if erlang {
  fn do_replace(
    iodata: StringBuilder,
    pattern: String,
    substitute: String,
  ) -> StringBuilder {
    erl_replace(iodata, pattern, substitute, All)
  }

  external fn erl_replace(
    StringBuilder,
    String,
    String,
    Direction,
  ) -> StringBuilder =
    "string" "replace"
}

if javascript {
  external fn do_replace(StringBuilder, String, String) -> StringBuilder =
    "../gleam_stdlib.mjs" "string_replace"
}

/// Compares two builders to determine if they have the same textual content.
///
/// Comparing two iodata using the `==` operator may return `False` even if they
/// have the same content as they may have been build in different ways, so
/// using this function is often preferred.
///
/// ## Examples
///
/// ```gleam
/// > from_strings(["a", "b"]) == from_string("ab")
/// False
/// ```
///
/// ```gleam
/// > is_equal(from_strings(["a", "b"]), from_string("ab"))
/// True
/// ```
///
pub fn is_equal(a: StringBuilder, b: StringBuilder) -> Bool {
  do_is_equal(a, b)
}

if erlang {
  external fn do_is_equal(StringBuilder, StringBuilder) -> Bool =
    "string" "equal"
}

if javascript {
  external fn do_is_equal(StringBuilder, StringBuilder) -> Bool =
    "../gleam_stdlib.mjs" "equal"
}

/// Inspects a builder to determine if it is equivalent to an empty string.
///
/// ## Examples
///
/// ```gleam
/// > from_string("ok") |> is_empty
/// False
/// ```
///
/// ```gleam
/// > from_string("") |> is_empty
/// True
/// ```
///
/// ```gleam
/// > from_strings([]) |> is_empty
/// True
/// ```
///
pub fn is_empty(builder: StringBuilder) -> Bool {
  do_is_empty(builder)
}

if erlang {
  external fn do_is_empty(StringBuilder) -> Bool =
    "string" "is_empty"
}

if javascript {
  fn do_is_empty(builder: StringBuilder) -> Bool {
    from_string("") == builder
  }
}
