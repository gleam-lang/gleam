pub opaque type Test {
  Test(name: String, proc: fn() -> Outcome)
  Suite(name: String, tests: List(Test))
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

pub opaque type Fail {
  Fail(detail: String)
}

pub type Outcome =
  Result(Pass, Fail)

pub fn pass() -> Outcome {
  Ok(Pass)
}

pub fn fail(detail: String) -> Outcome {
  Error(Fail(detail))
}

pub fn test_equal(name: String, left: a, right: a) -> Test {
  Test(name, fn() { equal(left, right) })
}

pub fn equal(left: a, right: a) -> Outcome {
  case left == right {
    True -> pass()
    False -> fail("Values were not equal")
  }
}

pub fn detail(outcome: Outcome, detail: String) -> Outcome {
  case outcome {
    Ok(Pass) -> pass()
    Error(_fail) -> fail(detail)
  }
}

pub type Printer =
  fn(String) -> String

pub type Stats {
  Stats(passes: Int, failures: Int)
}

pub fn run(tests: List(Test), print: Printer) -> Stats {
  run_list_of_tests(tests, print, Stats(0, 0), 0)
}

fn run_test(test: Test, print: Printer, stats: Stats, indentation: Int) -> Stats {
  case test {
    Test(name: name, proc: proc) ->
      run_single_test(name, proc, print, stats, indentation)
    Suite(name: name, tests: tests) ->
      run_suite(name, tests, print, stats, indentation)
  }
}

fn run_suite(name, tests, print, stats, indentation) {
  print_indentation(print, indentation)
  print(name)
  print("\n")
  run_list_of_tests(tests, print, stats, indentation + 1)
}

fn run_list_of_tests(tests, print, stats, indentation) {
  case tests {
    [] -> stats
    [test, ..tests] -> {
      let stats = run_test(test, print, stats, indentation)
      run_list_of_tests(tests, print, stats, indentation)
    }
  }
}

fn run_single_test(name, proc, print, stats, indentation) {
  print_indentation(print, indentation)
  case proc() {
    Ok(Pass) -> {
      print("✨ ")
      print(name)
      print("\n")
      Stats(..stats, passes: stats.passes + 1)
    }
    Error(Fail(detail)) -> {
      print("❌ ")
      print(name)
      print(": ")
      print(detail)
      print("\n")
      Stats(..stats, failures: stats.failures + 1)
    }
  }
}

fn print_indentation(print, indentation) {
  case indentation > 0 {
    True -> {
      print("  ")
      print_indentation(print, indentation - 1)
    }
    False -> Nil
  }
}
