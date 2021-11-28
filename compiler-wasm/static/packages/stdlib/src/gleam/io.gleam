/// Writes a string to standard output.
///
/// If you want your output to be printed on its own line see `println`.
///
/// ## Example
///
/// ```
/// > io.print("Hi mum")
/// // -> Hi mum
/// Nil
/// ```
///
pub fn print(string: String) -> Nil {
  do_print(string)
}

if erlang {
  fn do_print(string: String) -> Nil {
    erl_print("~s", [string])
    Nil
  }
}

if javascript {
  external fn do_print(String) -> Nil =
    "../gleam_stdlib.js" "print"
}

/// Writes a string to standard output, appending a newline to the end.
///
/// ## Example
///
///    > io.println("Hi mum")
///    // -> Hi mum
///    Nil
///
pub fn println(string: String) -> Nil {
  do_println(string)
}

if erlang {
  fn do_println(string: String) -> Nil {
    erl_print("~ts\n", [string])
    Nil
  }
}

if javascript {
  external fn do_println(String) -> Nil =
    "../gleam_stdlib.js" "log"
}

/// Prints a value to standard output using Erlang syntax.
///
/// The value is returned after being printed so it can be used in pipelines.
///
/// ## Example
///
///    > io.debug("Hi mum")
///    // -> <<"Hi mum">>
///    "Hi mum"
///
///    > io.debug(Ok(1))
///    // -> {ok, 1}
///    Ok(1)
///
///    > import list
///    > [1, 2]
///    > |> list.map(fn(x) { x + 1 })
///    > |> io.debug
///    > |> list.map(fn(x) { x * 2 })
///    // -> [2, 3]
///    [4, 6]
///
pub fn debug(term: anything) -> anything {
  debug_print(term)
  term
}

if erlang {
  fn debug_print(term: anything) -> DoNotLeak {
    erl_print("~tp\n", [term])
  }
}

if javascript {
  external fn debug_print(anything) -> Nil =
    "../gleam_stdlib.js" "debug"
}

if erlang {
  external type DoNotLeak

  external fn erl_print(String, List(a)) -> DoNotLeak =
    "io" "fwrite"
}
