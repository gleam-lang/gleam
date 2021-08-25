import ffi

pub opaque type Test {
  Example(name: String, proc: fn() -> Outcome)
  Suite(name: String, tests: List(Test))
}

pub fn example(name: String, proc: fn() -> Outcome) {
  Example(name, proc)
}

pub fn suite(name: String, tests: List(Test)) {
  Suite(name, tests)
}

pub opaque type Pass {
  Pass
}

pub opaque type Fail {
  Fail
}

pub type Outcome =
  Result(Pass, Fail)

pub fn pass() -> Outcome {
  Ok(Pass)
}

pub fn assert_equal(expected: a, got: a) -> Outcome {
  case expected == got {
    True -> pass()
    // TODO: print diff on failure
    _ -> Error(Fail)
  }
}

pub fn operator_test(operator_name, operator) {
  fn(a, b, expected) {
    let name =
      ffi.to_string(a)
      |> ffi.append(" ")
      |> ffi.append(operator_name)
      |> ffi.append(" ")
      |> ffi.append(ffi.to_string(b))
      |> ffi.append(" == ")
      |> ffi.append(ffi.to_string(expected))
    Example(name, fn() { assert_equal(operator(a, b), expected) })
  }
}

pub type ToString(anything) =
  fn(anything) -> String

pub type Printer =
  fn(String) -> String

pub type Stats {
  Stats(passes: Int, failures: Int)
}

pub fn run(tests: List(Test)) -> Stats {
  let stats = run_list_of_tests(tests, Stats(0, 0), 0)
  print_summary(stats)
  stats
}

fn print_summary(stats: Stats) {
  ffi.print("\n\n")
  ffi.print(ffi.to_string(stats.passes + stats.failures))
  ffi.print(" tests\n")
  ffi.print(ffi.to_string(stats.passes))
  ffi.print(" passes\n")
  ffi.print(ffi.to_string(stats.failures))
  ffi.print(" failures\n")
}

fn run_test(test: Test, stats: Stats, indentation: Int) -> Stats {
  case test {
    Example(name: name, proc: proc) ->
      run_single_test(name, proc, stats, indentation)
    Suite(name: name, tests: tests) ->
      run_suite(name, tests, stats, indentation)
  }
}

fn run_suite(name, tests, stats, indentation) {
  print_indentation(indentation)
  ffi.print("\n")
  ffi.print(name)
  ffi.print(" ")
  run_list_of_tests(tests, stats, indentation + 1)
}

fn run_list_of_tests(tests, stats, indentation) -> Stats {
  case tests {
    [] -> stats
    [test, ..tests] -> {
      let stats = run_test(test, stats, indentation)
      run_list_of_tests(tests, stats, indentation)
    }
  }
}

fn run_single_test(name, proc, stats, indentation) {
  case proc() {
    Ok(Pass) -> {
      ffi.print(ffi.ansi_green("."))
      Stats(..stats, passes: stats.passes + 1)
    }
    Error(Fail) -> {
      ffi.print("\n")
      print_indentation(indentation)
      ffi.print("❌ ")
      ffi.print(name)
      ffi.print(" failed!\n")
      print_indentation(indentation)
      // print_indentation(indentation)
      // ffi.print("expected: ")
      // ffi.print(ffi.to_string(expected))
      // ffi.print("\n")
      // print_indentation(indentation)
      // ffi.print("     got: ")
      // ffi.print(ffi.to_string(got))
      // ffi.print("\n")
      Stats(..stats, failures: stats.failures + 1)
    }
  }
}

fn print_indentation(indentation) {
  case indentation > 0 {
    True -> {
      ffi.print("  ")
      print_indentation(indentation - 1)
    }
    _ -> Nil
  }
}
