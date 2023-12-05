import gleam/string

/// Writes a string to standard output.
///
/// If you want your output to be printed on its own line see `println`.
///
/// ## Example
///
/// ```gleam
/// > io.print("Hi mum")
/// // -> Hi mum
/// Nil
/// ```
///
pub fn print(string: String) -> Nil {
  do_print(string)
}

@external(erlang, "gleam_stdlib", "print")
@external(javascript, "../gleam_stdlib.mjs", "print")
fn do_print(string string: String) -> Nil

/// Writes a string to standard error.
///
/// If you want your output to be printed on its own line see `println_error`.
///
/// ## Example
///
/// ```
/// > io.print_error("Hi pop")
/// // -> Hi pop
/// Nil
/// ```
///
pub fn print_error(string: String) -> Nil {
  do_print_error(string)
}

@external(erlang, "gleam_stdlib", "print_error")
@external(javascript, "../gleam_stdlib.mjs", "print_error")
fn do_print_error(string string: String) -> Nil

/// Writes a string to standard output, appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// > io.println("Hi mum")
/// // -> Hi mum
/// Nil
/// ```
///
pub fn println(string: String) -> Nil {
  do_println(string)
}

@external(erlang, "gleam_stdlib", "println")
@external(javascript, "../gleam_stdlib.mjs", "console_log")
fn do_println(string string: String) -> Nil

/// Writes a string to standard error, appending a newline to the end.
///
/// ## Example
///
/// ```gleam
/// > io.println_error("Hi pop")
/// // -> Hi mum
/// Nil
/// ```
///
pub fn println_error(string: String) -> Nil {
  do_println_error(string)
}

@external(erlang, "gleam_stdlib", "println_error")
@external(javascript, "../gleam_stdlib.mjs", "console_error")
fn do_println_error(string string: String) -> Nil

/// Prints a value to standard error (stderr) yielding Gleam syntax.
///
/// The value is returned after being printed so it can be used in pipelines.
///
/// ## Example
///
/// ```gleam
/// > debug("Hi mum")
/// // -> <<"Hi mum">>
/// "Hi mum"
/// ```
///
/// ```gleam
/// > debug(Ok(1))
/// // -> {ok, 1}
/// Ok(1)
/// ```
///
/// ```gleam
/// > import list
/// > [1, 2]
/// > |> list.map(fn(x) { x + 1 })
/// > |> debug
/// > |> list.map(fn(x) { x * 2 })
/// // -> [2, 3]
/// [4, 6]
/// ```
///
pub fn debug(term: anything) -> anything {
  term
  |> string.inspect
  |> do_debug_println

  term
}

@external(erlang, "gleam_stdlib", "println_error")
@external(javascript, "../gleam_stdlib.mjs", "print_debug")
fn do_debug_println(string string: String) -> Nil
