import test.{Test, suite, test_equal}

pub fn main(print: fn(String) -> String) -> Int {
  print("\nHere we go!\n\n")
  let stats =
    [suite("int", int_tests()), suite("float", float_tests())]
    |> test.run(print)

  case stats.failures {
    0 -> 0
    _ -> 1
  }
}

fn int_tests() -> List(Test) {
  [
    "addition"
    |> suite([
      test_equal("0 + 0", 0, 0 + 0),
      test_equal("1 + 1", 2, 1 + 1),
      test_equal("5 + 1", 6, 5 + 1),
      test_equal("1 + 3", 4, 1 + 3),
      test_equal("1 + -3", -2, 1 + -3),
    ]),
    "subtraction"
    |> suite([
      test_equal("0 - 0", 0, 0 - 0),
      test_equal("1 - 1", 0, 1 - 1),
      test_equal("5 - 1", 4, 5 - 1),
      test_equal("1 - 3", -2, 1 - 3),
      test_equal("1 - -3", 4, 1 - -3),
    ]),
  ]
}

fn float_tests() -> List(Test) {
  [
    "addition"
    |> suite([
      test_equal("0. +. 0.", 0., 0. +. 0.),
      test_equal("1. +. 1.", 2., 1. +. 1.),
      test_equal("5. +. 1.", 6., 5. +. 1.),
      test_equal("1. +. 3.", 4., 1. +. 3.),
      test_equal("1. +. -3.", -2., 1. +. -3.),
    ]),
    "subtraction"
    |> suite([
      test_equal("0. -. 0.", 0., 0. -. 0.),
      test_equal("1. -. 1.", 0., 1. -. 1.),
      test_equal("5. -. 1.", 4., 5. -. 1.),
      test_equal("1. -. 3.", -2., 1. -. 3.),
      test_equal("1. -. -3.", 4., 1. -. -3.),
      test_equal("0.5 -. 0.", 0.5, 0.5 -. 0.),
      test_equal("1. -. 4.4", -3.5, 1. -. 4.5),
    ]),
  ]
}
