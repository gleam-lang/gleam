import gleam/dynamic.{Dynamic}
import gleam/list
import gleam/erlang/atom.{Atom}
import gleam/erlang/charlist.{Charlist}

external fn erl_format(String, List(a)) -> Charlist =
  "io_lib" "format"

/// Return a string representation of any term
pub fn format(term: any) -> String {
  charlist.to_string(erl_format("~p", [term]))
}

pub external fn term_to_binary(a) -> BitString =
  "erlang" "term_to_binary"

type Safe {
  Safe
}

external fn erl_binary_to_term(BitString, List(Safe)) -> Dynamic =
  "erlang" "binary_to_term"

pub fn binary_to_term(binary: BitString) -> Result(Dynamic, Nil) {
  case rescue(fn() { erl_binary_to_term(binary, [Safe]) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

pub fn unsafe_binary_to_term(binary: BitString) -> Result(Dynamic, Nil) {
  case rescue(fn() { erl_binary_to_term(binary, []) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

/// Error value returned by `get_line` function
///
pub type GetLineError {
  Eof
  NoData
}

/// Reads a line from standard input with the given prompt.
///
/// # Example
///
///    > get_line("Language: ")
///    // -> Language: <- gleam
///    Ok("gleam\n")
///
pub external fn get_line(prompt: String) -> Result(String, GetLineError) =
  "gleam_erlang_ffi" "get_line"

pub type TimeUnit {
  Second
  Millisecond
  Microsecond
  Nanosecond
}

/// Returns the current OS system time.
///
/// <https://erlang.org/doc/apps/erts/time_correction.html#OS_System_Time>
pub external fn system_time(TimeUnit) -> Int =
  "os" "system_time"

/// Returns the current OS system time as a tuple of Ints
///
/// http://erlang.org/doc/man/os.html#timestamp-0
pub external fn erlang_timestamp() -> #(Int, Int, Int) =
  "os" "timestamp"

/// Gleam doesn't offer any way to raise exceptions, but they may still occur
/// due to bugs when working with unsafe code, such as when calling Erlang
/// function.
///
/// This function will catch any error thrown and convert it into a result
/// rather than crashing the process.
///
pub external fn rescue(fn() -> a) -> Result(a, Crash) =
  "gleam_erlang_ffi" "rescue"

pub type Crash {
  Exited(Dynamic)
  Thrown(Dynamic)
  Errored(Dynamic)
}

external fn get_start_arguments() -> List(Charlist) =
  "init" "get_plain_arguments"

/// Get the arguments given to the program when it was started.
///
/// This is sometimes called `argv` in other languages.
pub fn start_arguments() -> List(String) {
  get_start_arguments()
  |> list.map(charlist.to_string)
}

/// Starts an OTP application's process tree in the background, as well as
/// the trees of any applications that the given application depends upon. An
/// OTP application typically maps onto a Gleam or Hex package.
///
/// Returns a list of the applications that were started. Calling this function
/// for application that have already been started is a no-op so you do not need
/// to check the application state beforehand.
///
/// In Gleam we prefer to not use these implicit background process trees, but
/// you will likely still need to start the trees of OTP applications written in
/// other BEAM languages such as Erlang or Elixir, including those included by
/// default with Erlang/OTP.
///
/// For more information see the OTP documentation.
/// - <https://www.erlang.org/doc/man/application.html#ensure_all_started-1>
/// - <https://www.erlang.org/doc/man/application.html#start-1>
///
pub external fn ensure_all_started(
  application: Atom,
) -> Result(List(Atom), EnsureAllStartedError) =
  "gleam_erlang_ffi" "ensure_all_started"

pub type EnsureAllStartedError {
  UnknownApplication(name: Atom)
  ApplicationFailedToStart(name: Atom, reason: Dynamic)
}

/// A unique reference value.
///
/// It holds no particular meaning or value, but unique values are often useful
/// in programs are used heavily within both Gleam and Erlang's OTP frameworks.
///
/// More can be read about refernces in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/efficiency_guide/advanced.html#unique_references
///
pub external type Reference

/// Create a new unique reference.
///
pub external fn make_reference() -> Reference =
  "erlang" "make_ref"
