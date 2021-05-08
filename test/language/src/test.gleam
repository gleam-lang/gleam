pub opaque type Test {
  Test(name: String, proc: fn() -> Outcome)
  Suite(name: String, tests: List(Test))
}

pub type Functions(anything) {
  Functions(
    print: fn(String) -> String,
    to_string: ToString(anything),
    append: fn(String, String) -> String,
  )
}

pub fn test(name: String, proc: fn() -> Outcome) {
  Test(name, proc)
}

pub fn suite(name: String, tests: List(Test)) {
  Suite(name, tests)
}

pub opaque type Pass {
  Pass
}

external type Dynamic

external fn erase(a) -> Dynamic =
  "test" "identity"

pub fn identity(a) {
  a
}

pub opaque type Fail {
  Fail(expected: Dynamic, got: Dynamic)
}

pub type Outcome =
  Result(Pass, Fail)

pub fn pass() -> Outcome {
  Ok(Pass)
}

pub fn assert_equal(expected: a, got: a) -> Outcome {
  case expected == got {
    True -> pass()
    False -> Error(Fail(expected: erase(expected), got: erase(got)))
  }
}

pub fn operator_test(operator_name, operator, fns) {
  let Functions(append: append, to_string: to_string, ..) = fns
  fn(a, b, expected) {
    let name =
      to_string(a)
      |> append(" ")
      |> append(operator_name)
      |> append(" ")
      |> append(to_string(b))
      |> append(" == ")
      |> append(to_string(expected))
    test(name, fn() { assert_equal(operator(a, b), expected) })
  }
}

pub type ToString(anything) =
  fn(anything) -> String

pub type Printer =
  fn(String) -> String

pub type Stats {
  Stats(passes: Int, failures: Int)
}

pub fn run(tests: List(Test), fns: Functions(a)) -> Stats {
  run_list_of_tests(tests, fns, Stats(0, 0), 0)
}

fn run_test(
  test: Test,
  fns: Functions(a),
  stats: Stats,
  indentation: Int,
) -> Stats {
  case test {
    Test(name: name, proc: proc) ->
      run_single_test(name, proc, fns, stats, indentation)
    Suite(name: name, tests: tests) ->
      run_suite(name, tests, fns, stats, indentation)
  }
}

fn run_suite(name, tests, fns, stats, indentation) {
  print_indentation(fns, indentation)
  fns.print(name)
  fns.print("\n")
  run_list_of_tests(tests, fns, stats, indentation + 1)
}

fn run_list_of_tests(tests, fns, stats, indentation) {
  case tests {
    [] -> stats
    [test, ..tests] -> {
      let stats = run_test(test, fns, stats, indentation)
      run_list_of_tests(tests, fns, stats, indentation)
    }
  }
}

fn run_single_test(name, proc, fns, stats, indentation) {
  print_indentation(fns, indentation)
  case proc() {
    Ok(Pass) -> {
      fns.print("✨ ")
      fns.print(name)
      fns.print("\n")
      Stats(..stats, passes: stats.passes + 1)
    }
    Error(Fail(expected: expected, got: got)) -> {
      fns.print("❌ ")
      fns.print(name)
      fns.print("\n")
      print_indentation(fns, indentation)
      fns.print("expected: ")
      fns.print(fns.to_string(expected))
      fns.print("\n")
      print_indentation(fns, indentation)
      fns.print("     got: ")
      fns.print(fns.to_string(got))
      fns.print("\n")
      Stats(..stats, failures: stats.failures + 1)
    }
  }
}

fn print_indentation(fns: Functions(a), indentation) {
  case indentation > 0 {
    True -> {
      fns.print("  ")
      print_indentation(fns, indentation - 1)
    }
    False -> Nil
  }
}
