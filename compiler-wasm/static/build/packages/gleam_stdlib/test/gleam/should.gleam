//// A module for testing your Gleam code. The functions found here are
//// compatible with the Erlang eunit test framework.
////
//// More information on running eunit can be found in [the rebar3
//// documentation](https://rebar3.org/docs/testing/eunit/).

if erlang {
  pub external fn equal(a, a) -> Nil =
    "gleam_stdlib" "should_equal"

  pub external fn not_equal(a, a) -> Nil =
    "gleam_stdlib" "should_not_equal"

  pub external fn be_ok(Result(a, b)) -> Nil =
    "gleam_stdlib" "should_be_ok"

  pub external fn be_error(Result(a, b)) -> Nil =
    "gleam_stdlib" "should_be_error"
}

if javascript {
  import gleam/string

  external fn stringify(anything) -> String =
    "../gleam.js" "inspect"

  external fn crash(String) -> anything =
    "../gleam_stdlib.js" "crash"

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

  pub fn be_ok(a) {
    case a {
      Ok(_) -> Nil
      _ -> crash(string.concat(["\n", stringify(a), "\nshould be ok"]))
    }
  }

  pub fn be_error(a) {
    case a {
      Error(_) -> Nil
      _ -> crash(string.concat(["\n", stringify(a), "\nshould be error"]))
    }
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
