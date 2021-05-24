import test.{Functions, Test, example, operator_test, suite}
import importable.{NoFields}

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
      suite("constants", constants_tests(fns)),
      suite("imported custom types", imported_custom_types_test(fns)),
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

const const_int = 5

const const_float = 1.0

const const_string = "Gleam"

const const_nil = Nil

const const_ok = Ok(1)

const const_list_empty = []

const const_list_1 = [1]

const const_list_2 = [1, 2]

fn constants_tests(_fns) -> List(Test) {
  [
    equality_test("int", const_int, 5),
    equality_test("float", const_float, 1.0),
    equality_test("string", const_string, "Gleam"),
    equality_test("Nil", const_nil, Nil),
    equality_test("Ok", const_ok, Ok(1)),
    equality_test("list empty", const_list_empty, []),
    equality_test("list 1", const_list_1, [1]),
    equality_test("list 2", const_list_2, [1, 2]),
  ]
}

fn imported_custom_types_test(_fns) -> List(Test) {
  [
    equality_test(
      "No fields, qualified and unqualified",
      importable.NoFields,
      NoFields,
    ),
    lazy_equality_test(
      "No fields assignment",
      fn() { assert importable.NoFields = importable.NoFields },
      importable.NoFields,
    ),
    lazy_equality_test(
      "No fields unqualified assignment",
      fn() { assert NoFields = importable.NoFields },
      importable.NoFields,
    ),
  ]
}

fn equality_test(name: String, left: a, right: a) {
  example(name, fn() { test.assert_equal(left, right) })
}

fn lazy_equality_test(name: String, left: fn() -> a, right: a) {
  example(name, fn() { test.assert_equal(left(), right) })
}
