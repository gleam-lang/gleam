import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}

@external(erlang, "io_lib", "format")
fn erl_format(a: String, b: List(a)) -> Charlist

/// Return a string representation of any term
pub fn format(term: any) -> String {
  charlist.to_string(erl_format("~p", [term]))
}

@external(erlang, "erlang", "term_to_binary")
pub fn term_to_binary(a: a) -> BitArray

type Safe {
  Safe
}

@external(erlang, "erlang", "binary_to_term")
fn erl_binary_to_term(a: BitArray, b: List(Safe)) -> Dynamic

pub fn binary_to_term(binary: BitArray) -> Result(Dynamic, Nil) {
  case rescue(fn() { erl_binary_to_term(binary, [Safe]) }) {
    Ok(term) -> Ok(term)
    Error(_) -> Error(Nil)
  }
}

pub fn unsafe_binary_to_term(binary: BitArray) -> Result(Dynamic, Nil) {
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
@external(erlang, "gleam_erlang_ffi", "get_line")
pub fn get_line(prompt prompt: String) -> Result(String, GetLineError)

pub type TimeUnit {
  Second
  Millisecond
  Microsecond
  Nanosecond
}

/// Returns the current OS system time.
///
/// <https://erlang.org/doc/apps/erts/time_correction.html#OS_System_Time>
@external(erlang, "os", "system_time")
pub fn system_time(a: TimeUnit) -> Int

/// Returns the current OS system time as a tuple of Ints
///
/// http://erlang.org/doc/man/os.html#timestamp-0
@external(erlang, "os", "timestamp")
pub fn erlang_timestamp() -> #(Int, Int, Int)

/// Gleam doesn't offer any way to raise exceptions, but they may still occur
/// due to bugs when working with unsafe code, such as when calling Erlang
/// function.
///
/// This function will catch any error thrown and convert it into a result
/// rather than crashing the process.
///
@external(erlang, "gleam_erlang_ffi", "rescue")
pub fn rescue(a: fn() -> a) -> Result(a, Crash)

pub type Crash {
  Exited(Dynamic)
  Thrown(Dynamic)
  Errored(Dynamic)
}

@external(erlang, "init", "get_plain_arguments")
fn get_start_arguments() -> List(Charlist)

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
@external(erlang, "gleam_erlang_ffi", "ensure_all_started")
pub fn ensure_all_started(
  application application: Atom,
) -> Result(List(Atom), EnsureAllStartedError)

pub type EnsureAllStartedError {
  UnknownApplication(name: Atom)
  ApplicationFailedToStart(name: Atom, reason: Dynamic)
}

/// A unique reference value.
///
/// It holds no particular meaning or value, but unique values are often useful
/// in programs are used heavily within both Gleam and Erlang's OTP frameworks.
///
/// More can be read about references in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/efficiency_guide/advanced.html#unique_references
///
pub type Reference

/// Create a new unique reference.
///
@external(erlang, "erlang", "make_ref")
pub fn make_reference() -> Reference

/// Returns the path of a package's `priv` directory, where extra non-Gleam
/// or Erlang files are typically kept.
///
/// Returns an error if no package was found with the given name.
///
/// # Example
///
/// ```gleam
/// > erlang.priv_directory("my_app")
/// // -> Ok("/some/location/my_app/priv")
/// ```
/// 
@external(erlang, "gleam_erlang_ffi", "priv_directory")
pub fn priv_directory(name: String) -> Result(String, Nil)
