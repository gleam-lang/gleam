import test.{Functions, Test, example, operator_test, suite}

pub fn main(
  print: fn(String) -> String,
  to_string: fn(anything) -> String,
  append: fn(String, String) -> String,
) -> Int {
  print("\nHere we go!\n\n")

  let fns = Functions(print, to_string, append)
  let stats =
    [
      suite("int", int_tests(fns)),
      suite("float", float_tests(fns)),
      suite("tail call optimisation", tail_call_optimisation_tests(fns)),
    ]
    |> test.run(fns)

  case stats.failures {
    0 -> 0
    _ -> 1
  }
}

fn int_tests(fns) -> List(Test) {
  let addition = operator_test("+", fn(a, b) { a + b }, fns)
  let subtraction = operator_test("-", fn(a, b) { a - b }, fns)
  [
    "addition"
    |> suite([
      addition(0, 0, 0),
      addition(1, 1, 2),
      addition(5, 1, 6),
      addition(1, 3, 4),
      addition(1, -3, -2),
    ]),
    "subtraction"
    |> suite([
      subtraction(0, 0, 0),
      subtraction(1, 1, 0),
      subtraction(5, 1, 4),
      subtraction(1, 3, -2),
      subtraction(1, -3, 4),
    ]),
  ]
}

fn float_tests(fns) -> List(Test) {
  let addition = operator_test("+.", fn(a, b) { a +. b }, fns)
  let subtraction = operator_test("-.", fn(a, b) { a -. b }, fns)
  [
    "addition"
    |> suite([
      addition(0., 0., 0.),
      addition(1., 1., 2.),
      addition(5., 1., 6.),
      addition(1., 3., 4.),
      addition(1., -3., -2.),
    ]),
    "subtraction"
    |> suite([
      subtraction(0., 0., 0.),
      subtraction(1., 1., 0.),
      subtraction(5., 1., 4.),
      subtraction(1., 3., -2.),
      subtraction(1., -3., 4.),
      subtraction(0.5, 0., 0.5),
      subtraction(1., 4.5, -3.5),
    ]),
  ]
}

fn tail_call_optimisation_tests(_fns) -> List(Test) {
  [
    example(
      "10 million recursions doesn't overflow the stack",
      fn() { test.assert_equal(Nil, count_down(from: 10_000_000)) },
    ),
  ]
}

fn count_down(from i) {
  case i {
    0 -> Nil
    _ -> count_down(i - 1)
  }
}
