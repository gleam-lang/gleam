//// 
//// - **Outputs**
////   - [`error`](#error)
////   - [`warning`](#error)
////   - [`info`](#info)
////
//// ---
////
//// <details>
//// <summary>The license of that package is produced below:</summary>
//// 
//// 
//// > MIT License
////
//// > Permission is hereby granted, free of charge, to any person obtaining a copy
//// of this software and associated documentation files (the "Software"), to deal
//// in the Software without restriction, including without limitation the rights
//// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//// copies of the Software, and to permit persons to whom the Software is
//// furnished to do so, subject to the following conditions:
////
//// > The above copyright notice and this permission notice shall be included in all
//// copies or substantial portions of the Software.
//// </details>
////

// IMPORTS --------------------------------------------------------------------

import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam_community/ansi

// TYPES ----------------------------------------------------------------------

type Location {
  Location(row: Int, col: Int)
}

type Output {
  Error
  Warning
  Info
}

// OUTPUTS --------------------------------------------------------------------

/// Returns a `String` displaying the provided error and relevant code.
///
///
/// <details>
/// <summary>Example:</summary>
///
/// ```gleam
/// fn example() {
///   let source = "let five = 4 + 1.0"
///   
///   source
///   |> hug.error(
///     in: "example.gleam",
///     from: #(1, 11),
///     to: #(1, 18),
///     message: "invalid type",
///     hint: "can not add an `Int` to a `Float`"
///   )
///   |> io.println()
/// }
/// ```
/// </details>
///
/// <div style="position: relative;">
///     <a style="position: absolute; left: 0;" href="https://github.com/brettkolodny/hug/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
///     <a style="position: absolute; right: 0;" href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn error(
  in file_name: String,
  containing source: String,
  from start: #(Int, Int),
  to end: #(Int, Int),
  message msg: String,
  hint hint: String,
) -> String {
  output(
    file_name,
    source,
    Location(row: start.0, col: start.1),
    Location(row: end.0, col: end.1),
    msg,
    hint,
    Error,
  )
}

/// Returns a `String` displaying the provided warning and relevant code.
///
///
/// <details>
/// <summary>Example:</summary>
///
/// ```gleam
/// fn example() {
///   let source = "let five = 4 + 1.0"
///   
///   source
///   |> hug.warning(
///     in: "example.gleam",
///     from: #(1, 5),
///     to: #(1, 9),
///     message: "unused variable",
///     hint: "the variable `five` is declared but never used"
///   )
///   |> io.println()
/// }
/// ```
/// </details>
///
/// <div style="position: relative;">
///     <a style="position: absolute; left: 0;" href="https://github.com/brettkolodny/hug/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
///     <a style="position: absolute; right: 0;" href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn warning(
  in file_name: String,
  containing source: String,
  from start: #(Int, Int),
  to end: #(Int, Int),
  message msg: String,
  hint hint: String,
) -> String {
  output(
    file_name,
    source,
    Location(row: start.0, col: start.1),
    Location(row: end.0, col: end.1),
    msg,
    hint,
    Warning,
  )
}

/// Returns a `String` displaying the provided info and relevant code.
///
///
/// <details>
/// <summary>Example:</summary>
///
/// ```gleam
/// fn example() {
///   let source = "try five = int.parse("4") |> result.then(fn(v) { v + 1})"
///   
///   source
///   |> hug.info(
///     in: "example.gleam",
///     from: #(1, 1),
///     to: #(1, 4),
///     message: "use of deprecated code",
///     hint: "`try` is marked as deprecated, check out `use`!"
///   )
///   |> io.println()
/// }
/// ```
/// </details>
///
/// <div style="position: relative;">
///     <a style="position: absolute; left: 0;" href="https://github.com/brettkolodny/hug/issues">
///         <small>Spot a typo? Open an issue!</small>
///     </a>
///     <a style="position: absolute; right: 0;" href="#">
///         <small>Back to top ↑</small>
///     </a>
/// </div>
///
pub fn info(
  in file_name: String,
  containing source: String,
  from start: #(Int, Int),
  to end: #(Int, Int),
  message msg: String,
  hint hint: String,
) -> String {
  output(
    file_name,
    source,
    Location(row: start.0, col: start.1),
    Location(row: end.0, col: end.1),
    msg,
    hint,
    Info,
  )
}

// ----------------------------------------------------------------------------

fn output(
  file_name: String,
  source: String,
  start: Location,
  end: Location,
  err: String,
  hint: String,
  output: Output,
) {
  let header = construct_header(err, output)

  let body = construct_body(file_name, source, start, end, output)

  string.join([header, body, "", hint], "\n")
}

//
fn get_relevant_lines(
  source_lines: List(String),
  start: Location,
  end: Location,
) -> List(String) {
  use lines, line, index <- list.index_fold(source_lines, [])

  case index + 1 >= start.row && index + 1 <= end.row {
    True -> list.append(lines, [line])
    False -> lines
  }
}

//
fn underline_source(
  source_lines: List(String),
  start: Location,
  end: Location,
  output: Output,
) -> List(String) {
  let colour = case output {
    Error -> ansi.red
    Warning -> ansi.yellow
    Info -> ansi.blue
  }

  use index, line <- list.index_map(source_lines)

  case string.trim(line) {
    "" -> ""
    _ ->
      case index == 0 {
        True -> {
          let white_space = string.repeat(" ", start.col - 1)

          let underline_end = case end.row == start.row {
            True -> end.col - start.col
            False -> string.length(line) - string.length(white_space)
          }

          white_space <> colour(string.repeat("~", underline_end))
        }

        False -> {
          let line_length = string.length(line)
          let line_length_post_trim = string.length(string.trim_left(line))

          let num_white_space = line_length - line_length_post_trim

          let white_space = string.repeat(" ", num_white_space)

          white_space <> colour(string.repeat("~", line_length_post_trim))
        }
      }
  }
}

fn construct_header(message: String, output: Output) -> String {
  case output {
    Error -> ansi.red("error: ") <> message
    Warning -> ansi.yellow("warning: ") <> message
    Info -> ansi.blue("info: ") <> message
  }
}

//
fn construct_body(
  file_name: String,
  source: String,
  start: Location,
  end: Location,
  output: Output,
) -> String {
  let left_padding =
    int.max(
      string.length(int.to_string(start.row)),
      string.length(int.to_string(end.row)),
    ) - 1

  let body_start =
    string.repeat(" ", left_padding) <> "  ┌─ " <> file_name <> ":" <> int.to_string(
      start.row,
    ) <> ":" <> int.to_string(start.col)

  let relevant_lines =
    get_relevant_lines(string.split(source, on: "\n"), start, end)

  let underlines = underline_source(relevant_lines, start, end, output)

  let trim_left_amount = get_trim_left_amount(relevant_lines)

  let body =
    list.zip(relevant_lines, underlines)
    |> list.index_map(fn(index, input) {
      construct_output_line(
        input,
        index + start.row,
        trim_left_amount,
        left_padding,
      )
    })
    |> string.join("\n")

  string.join(
    [
      body_start,
      string.repeat(" ", left_padding) <> "  │",
      body,
      string.repeat(" ", left_padding) <> "  │",
    ],
    "\n",
  )
}

//
fn construct_output_line(
  input: #(String, String),
  row: Int,
  trim_left_amount: Int,
  left_padding: Int,
) -> String {
  let #(source_line, underline) = input

  let line_number_padding = left_padding - string.length(int.to_string(row)) + 1

  let source_line =
    ansi.green(int.to_string(row)) <> string.repeat(" ", line_number_padding) <> " │ " <> trim_left(
      source_line,
      by: trim_left_amount,
    )

  case string.length(underline) {
    0 -> source_line
    _ -> {
      let underline_line =
        string.repeat(" ", left_padding) <> "  │ " <> trim_left(
          underline,
          by: trim_left_amount,
        )

      string.join([source_line, underline_line], "\n")
    }
  }
}

//
fn get_trim_left_amount(lines: List(String)) -> Int {
  let get_left_white_space = fn(line) {
    string.length(line) - {
      line
      |> string.trim_left()
      |> string.length()
    }
  }

  let first_line =
    list.first(lines)
    |> result.unwrap("")

  use min_white_space, line <- list.fold(
    lines,
    get_left_white_space(first_line),
  )

  case string.trim(line) {
    "" -> min_white_space
    _ -> {
      let white_space =
        string.length(line) - {
          line
          |> string.trim_left()
          |> string.length()
        }

      int.min(min_white_space, white_space)
    }
  }
}

//
fn trim_left(str: String, by num_white_space: Int) -> String {
  let string_length = string.length(str)

  string.slice(from: str, at_index: num_white_space, length: string_length)
}
