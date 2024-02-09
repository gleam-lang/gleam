/// Find and run all test functions for the current project using Erlang's EUnit
/// test framework.
///
/// Any Erlang or Gleam function in the `test` directory with a name editing in
/// `_test` is considered a test function and will be run.
///
/// If running on JavaScript tests will be run with a custom test runner.
///
pub fn main() -> Nil {
  do_main()
}

@target(javascript)
@external(javascript, "./gleeunit_ffi.mjs", "main")
fn do_main() -> Nil

@target(erlang)
import gleam/list
@target(erlang)
import gleam/result
@target(erlang)
import gleam/string
@target(erlang)
import gleam/dynamic.{Dynamic}

@target(erlang)
fn do_main() -> Nil {
  let options = [Verbose, NoTty, Report(#(GleeunitProgress, [Colored(True)]))]

  let result =
    find_files(matching: "**/*.{erl,gleam}", in: "test")
    |> list.map(gleam_to_erlang_module_name)
    |> list.map(dangerously_convert_string_to_atom(_, Utf8))
    |> run_eunit(options)
    |> dynamic.result(dynamic.dynamic, dynamic.dynamic)
    |> result.unwrap(Error(dynamic.from(Nil)))

  let code = case result {
    Ok(_) -> 0
    Error(_) -> 1
  }
  halt(code)
}

@target(erlang)
@external(erlang, "erlang", "halt")
fn halt(a: Int) -> Nil

@target(erlang)
fn gleam_to_erlang_module_name(path: String) -> String {
  path
  |> string.replace(".gleam", "")
  |> string.replace(".erl", "")
  |> string.replace("/", "@")
}

@target(erlang)
@external(erlang, "gleeunit_ffi", "find_files")
fn find_files(matching matching: String, in in: String) -> List(String)

@target(erlang)
type Atom

@target(erlang)
type Encoding {
  Utf8
}

@target(erlang)
@external(erlang, "erlang", "binary_to_atom")
fn dangerously_convert_string_to_atom(a: String, b: Encoding) -> Atom

@target(erlang)
type ReportModuleName {
  GleeunitProgress
}

@target(erlang)
type GleeunitProgressOption {
  Colored(Bool)
}

@target(erlang)
type EunitOption {
  Verbose
  NoTty
  Report(#(ReportModuleName, List(GleeunitProgressOption)))
}

@target(erlang)
@external(erlang, "eunit", "test")
fn run_eunit(a: List(Atom), b: List(EunitOption)) -> Dynamic
