//// A module for testing your Gleam code. The functions found here are
//// compatible with the Erlang eunit test framework.
////
//// More information on running eunit can be found in [the rebar3
//// documentation](https://rebar3.org/docs/testing/eunit/).

@target(erlang)
@external(erlang, "gleeunit_ffi", "should_equal")
pub fn equal(a: a, b: a) -> Nil

@target(erlang)
@external(erlang, "gleeunit_ffi", "should_not_equal")
pub fn not_equal(a: a, b: a) -> Nil

@target(erlang)
@external(erlang, "gleeunit_ffi", "should_be_ok")
pub fn be_ok(a: Result(a, b)) -> a

@target(erlang)
@external(erlang, "gleeunit_ffi", "should_be_error")
pub fn be_error(a: Result(a, b)) -> b

@target(javascript)
import gleam/string

@target(javascript)
@external(javascript, "../gleam.mjs", "inspect")
fn stringify(a: anything) -> String

@target(javascript)
@external(javascript, "../gleeunit_ffi.mjs", "crash")
fn crash(a: String) -> anything

@target(javascript)
pub fn equal(a, b) {
  case a == b {
    True -> Nil
    _ ->
      crash(string.concat([
        "\n\t",
        stringify(a),
        "\n\tshould equal \n\t",
        stringify(b),
      ]))
  }
}

@target(javascript)
pub fn not_equal(a, b) {
  case a != b {
    True -> Nil
    _ ->
      crash(string.concat([
        "\n",
        stringify(a),
        "\nshould not equal \n",
        stringify(b),
      ]))
  }
}

@target(javascript)
pub fn be_ok(a) {
  case a {
    Ok(value) -> value
    _ -> crash(string.concat(["\n", stringify(a), "\nshould be ok"]))
  }
}

@target(javascript)
pub fn be_error(a) {
  case a {
    Error(error) -> error
    _ -> crash(string.concat(["\n", stringify(a), "\nshould be error"]))
  }
}

pub fn be_true(actual: Bool) -> Nil {
  actual
  |> equal(True)
}

pub fn be_false(actual: Bool) -> Nil {
  actual
  |> equal(False)
}

pub fn fail() -> Nil {
  be_true(False)
}
