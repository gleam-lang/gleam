//// A charlist is a list of integers where all the integers are valid code
//// points.
////
//// In practice, you will not come across them often, except perhaps when
//// interfacing with Erlang, in particular when using older libraries that do
//// not accept binaries as arguments.

/// A list of characters represented as ints. Commonly used by older Erlang
/// modules.
pub external type Charlist

/// Transform a charlist to a string
pub external fn to_string(Charlist) -> String =
  "unicode" "characters_to_binary"

// Calls `unicode:characters_to_binary(Data, unicode, unicode)`
// Note: `unicode is an alias for utf8`
// See <https://www.erlang.org/doc/man/unicode.html#characters_to_binary-1>

/// Transform a string to a charlist
pub external fn from_string(String) -> Charlist =
  "unicode" "characters_to_list"
// Calls `unicode:characters_to_list(Data, unicode)`
// Note: `unicode is an alias for utf8`
// See <https://www.erlang.org/doc/man/unicode.html#characters_to_list-1>
