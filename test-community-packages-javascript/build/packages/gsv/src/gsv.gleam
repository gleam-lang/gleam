import gsv/internal/ast
import gsv/internal/token
import gleam/list
import gleam/string

/// Parses a csv string to a list of lists of strings.
/// Automatically handles Windows and Unix line endings.
pub fn to_lists(input: String) -> Result(List(List(String)), Nil) {
  input
  |> token.scan
  |> ast.parse
}

/// Option for using "\n = LF = Unix" or "\r\n = CRLF = Windows"
/// line endings. Use with the `from_lists` function when 
/// writing to a csv string.
pub type LineEnding {
  Windows
  Unix
}

fn le_to_string(le: LineEnding) -> String {
  case le {
    Windows -> "\r\n"
    Unix -> "\n"
  }
}

/// Takes a list of lists of strings and writes it to a csv string.
/// Will automatically escape strings that contain double quotes or
/// line endings with double quotes (in csv, double quotes get escaped by doing
/// a double doublequote)
/// The string `he"llo\n` becomes `"he""llo\n"`
pub fn from_lists(
  input: List(List(String)),
  separator separator: String,
  line_ending line_ending: LineEnding,
) -> String {
  input
  |> list.map(fn(row) {
    list.map(
      row,
      fn(entry) {
        // Double quotes need to be escaped with an extra doublequote
        let entry = string.replace(entry, "\"", "\"\"")

        // If the string contains a , \n \r\n or " it needs to be escaped by wrapping in double quotes
        case
          string.contains(entry, separator) || string.contains(entry, "\n") || string.contains(
            entry,
            "\"",
          )
        {
          True -> "\"" <> entry <> "\""
          False -> entry
        }
      },
    )
  })
  |> list.map(fn(row) { string.join(row, separator) })
  |> string.join(le_to_string(line_ending))
}
