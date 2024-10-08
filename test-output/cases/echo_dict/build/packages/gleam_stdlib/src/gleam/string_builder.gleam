import gleam/list

/// `StringBuilder` is a type used for efficiently building text content to be
/// written to a file or a socket. Internally it is represented as tree so to
/// append or prepend to a string builder is a constant time operation that
/// allocates a new node in the tree without copying any of the content. When
/// writing to an output stream the tree is traversed and the content is sent
/// directly rather than copying it into a single buffer beforehand.
///
/// On Erlang this type is compatible with Erlang's iodata. On JavaScript this
/// type is compatible with normal strings.
///
/// The BEAM virtual machine has an optimisation for appending strings, where it
/// will mutate the string buffer when safe to do so, so if you are looking to
/// build a string through appending many small strings then you may get better
/// performance by not using a string builder. Always benchmark your performance
/// sensitive code.
///
pub type StringBuilder

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

@external(erlang, "gleam_stdlib", "iodata_append")
@external(javascript, "../gleam_stdlib.mjs", "add")
fn do_append(a: StringBuilder, b: StringBuilder) -> StringBuilder

/// Converts a list of strings into a builder.
///
/// Runs in constant time.
///
pub fn from_strings(strings: List(String)) -> StringBuilder {
  do_from_strings(strings)
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "concat")
fn do_from_strings(a: List(String)) -> StringBuilder

/// Joins a list of builders into a single builder.
///
/// Runs in constant time.
///
pub fn concat(builders: List(StringBuilder)) -> StringBuilder {
  do_concat(builders)
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "concat")
fn do_concat(builders: List(StringBuilder)) -> StringBuilder

/// Converts a string into a builder.
///
/// Runs in constant time.
///
pub fn from_string(string: String) -> StringBuilder {
  do_from_string(string)
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_from_string(string: String) -> StringBuilder

/// Turns an `StringBuilder` into a `String`
///
/// This function is implemented natively by the virtual machine and is highly
/// optimised.
///
pub fn to_string(builder: StringBuilder) -> String {
  do_to_string(builder)
}

@external(erlang, "unicode", "characters_to_binary")
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn do_to_string(builder: StringBuilder) -> String

/// Returns the size of the `StringBuilder` in bytes.
///
pub fn byte_size(builder: StringBuilder) -> Int {
  do_byte_size(builder)
}

@external(erlang, "erlang", "iolist_size")
@external(javascript, "../gleam_stdlib.mjs", "length")
fn do_byte_size(builder: StringBuilder) -> Int

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

@external(erlang, "string", "lowercase")
@external(javascript, "../gleam_stdlib.mjs", "lowercase")
fn do_lowercase(builder: StringBuilder) -> StringBuilder

/// Converts a builder to a new builder where the contents have been
/// uppercased.
///
pub fn uppercase(builder: StringBuilder) -> StringBuilder {
  do_uppercase(builder)
}

@external(erlang, "string", "uppercase")
@external(javascript, "../gleam_stdlib.mjs", "uppercase")
fn do_uppercase(builder: StringBuilder) -> StringBuilder

/// Converts a builder to a new builder with the contents reversed.
///
pub fn reverse(builder: StringBuilder) -> StringBuilder {
  do_reverse(builder)
}

@external(erlang, "string", "reverse")
fn do_reverse(builder: StringBuilder) -> StringBuilder {
  builder
  |> to_string
  |> do_to_graphemes
  |> list.reverse
  |> from_strings
}

@external(javascript, "../gleam_stdlib.mjs", "graphemes")
fn do_to_graphemes(string: String) -> List(String)

/// Splits a builder on a given pattern into a list of builders.
///
pub fn split(iodata: StringBuilder, on pattern: String) -> List(StringBuilder) {
  do_split(iodata, pattern)
}

type Direction {
  All
}

@external(javascript, "../gleam_stdlib.mjs", "split")
fn do_split(iodata: StringBuilder, pattern: String) -> List(StringBuilder) {
  erl_split(iodata, pattern, All)
}

@external(erlang, "string", "split")
fn erl_split(a: StringBuilder, b: String, c: Direction) -> List(StringBuilder)

/// Replaces all instances of a pattern with a given string substitute.
///
@external(erlang, "gleam_stdlib", "string_replace")
@external(javascript, "../gleam_stdlib.mjs", "string_replace")
pub fn replace(
  in builder: StringBuilder,
  each pattern: String,
  with substitute: String,
) -> StringBuilder

/// Compares two builders to determine if they have the same textual content.
///
/// Comparing two iodata using the `==` operator may return `False` even if they
/// have the same content as they may have been build in different ways, so
/// using this function is often preferred.
///
/// ## Examples
///
/// ```gleam
/// from_strings(["a", "b"]) == from_string("ab")
/// // -> False
/// ```
///
/// ```gleam
/// is_equal(from_strings(["a", "b"]), from_string("ab"))
/// // -> True
/// ```
///
@external(erlang, "string", "equal")
pub fn is_equal(a: StringBuilder, b: StringBuilder) -> Bool {
  a == b
}

/// Inspects a builder to determine if it is equivalent to an empty string.
///
/// ## Examples
///
/// ```gleam
/// from_string("ok") |> is_empty
/// // -> False
/// ```
///
/// ```gleam
/// from_string("") |> is_empty
/// // -> True
/// ```
///
/// ```gleam
/// from_strings([]) |> is_empty
/// // -> True
/// ```
///
@external(erlang, "string", "is_empty")
pub fn is_empty(builder: StringBuilder) -> Bool {
  from_string("") == builder
}
