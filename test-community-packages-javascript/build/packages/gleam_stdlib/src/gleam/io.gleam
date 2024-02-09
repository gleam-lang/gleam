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

if erlang {
  external fn do_print(string: String) -> Nil =
    "gleam_stdlib" "print"
}

if javascript {
  external fn do_print(String) -> Nil =
    "../gleam_stdlib.mjs" "print"
}

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

if erlang {
  external fn do_print_error(string: String) -> Nil =
    "gleam_stdlib" "print_error"
}

if javascript {
  external fn do_print_error(String) -> Nil =
    "../gleam_stdlib.mjs" "print_error"
}

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

if erlang {
  external fn do_println(string: String) -> Nil =
    "gleam_stdlib" "println"
}

if javascript {
  external fn do_println(String) -> Nil =
    "../gleam_stdlib.mjs" "console_log"
}

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

if erlang {
  external fn do_println_error(string: String) -> Nil =
    "gleam_stdlib" "println_error"
}

if javascript {
  external fn do_println_error(String) -> Nil =
    "../gleam_stdlib.mjs" "console_error"
}

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

if erlang {
  external fn do_debug_println(string: String) -> Nil =
    "gleam_stdlib" "println_error"
}

if javascript {
  external fn do_debug_println(String) -> Nil =
    "../gleam_stdlib.mjs" "print_debug"
}
