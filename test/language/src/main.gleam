import test.{Functions, Test, assert_equal, example, operator_test, suite}
import importable.{NoFields}

pub fn main(
  print: fn(String) -> String,
  to_string: fn(anything) -> String,
  append: fn(String, String) -> String,
) -> Int {
  let fns = Functions(print, to_string, append)
  let stats =
    [
      suite("try", try_tests(fns)),
      suite("int", int_tests(fns)),
      suite("float", float_tests(fns)),
      suite("pipes", pipes_tests(fns)),
      suite("constants", constants_tests(fns)),
      suite("clause guards", clause_guard_tests(fns)),
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
  let basic_addition = operator_test("+", fn(a, b) { a + b }, fns)
  let basic_subtraction = operator_test("-", fn(a, b) { a - b }, fns)
  let basic_multiplication = operator_test("*", fn(a, b) { a * b }, fns)
  [
    basic_addition(0, 0, 0),
    basic_addition(1, 1, 2),
    basic_addition(5, 1, 6),
    basic_addition(1, 3, 4),
    basic_addition(1, -3, -2),
    basic_subtraction(0, 0, 0),
    basic_subtraction(1, 1, 0),
    basic_subtraction(5, 1, 4),
    basic_subtraction(1, 3, -2),
    basic_subtraction(1, -3, 4),
    basic_multiplication(0, 0, 0),
    basic_multiplication(1, 1, 1),
    basic_multiplication(2, 2, 4),
    basic_multiplication(2, 4, 8),
    basic_multiplication(-2, 4, -8),
    basic_multiplication(2, -4, -8),
    equality_test("Precedence 0", 7, 2 * 2 + 3),
    equality_test("Precedence 1", 8, 2 + 2 * 3),
    equality_test("Precedence 2", 10, 2 * { 2 + 3 }),
    equality_test("Precedence 3", 12, { 2 + 2 } * 3),
  ]
}

fn float_tests(fns) -> List(Test) {
  let basic_addition = operator_test("+.", fn(a, b) { a +. b }, fns)
  let basic_subtraction = operator_test("-.", fn(a, b) { a -. b }, fns)
  let basic_multiplication = operator_test("*.", fn(a, b) { a *. b }, fns)
  [
    basic_addition(0., 0., 0.),
    basic_addition(1., 1., 2.),
    basic_addition(5., 1., 6.),
    basic_addition(1., 3., 4.),
    basic_addition(1., -3., -2.),
    basic_subtraction(0., 0., 0.),
    basic_subtraction(1., 1., 0.),
    basic_subtraction(5., 1., 4.),
    basic_subtraction(1., 3., -2.),
    basic_subtraction(1., -3., 4.),
    basic_subtraction(0.5, 0., 0.5),
    basic_subtraction(1., 4.5, -3.5),
    basic_multiplication(0.0, 0.0, 0.0),
    basic_multiplication(1.0, 1.0, 1.0),
    basic_multiplication(2.0, 2.0, 4.0),
    basic_multiplication(2.0, 4.0, 8.0),
    basic_multiplication(-2.0, 4.0, -8.0),
    basic_multiplication(2.0, -4.0, -8.0),
    equality_test("Precedence 0", 7.0, 2.0 *. 2.0 +. 3.0),
    equality_test("Precedence 1", 8.0, 2.0 +. 2.0 *. 3.0),
    equality_test("Precedence 2", 10.0, 2.0 *. { 2.0 +. 3.0 }),
    equality_test("Precedence 3", 12.0, { 2.0 +. 2.0 } *. 3.0),
  ]
}

fn identity(x) {
  x
}

fn pair(x, y) {
  #(x, y)
}

fn triplet(x x, y y, z z) {
  #(x, y, z)
}

fn pipes_tests(_fns) -> List(Test) {
  [
    "pipe last"
    |> example(fn() {
      let result =
        100
        |> identity
      assert_equal(100, result)
    }),
    "pipe into anon"
    |> example(fn() {
      let result =
        100
        |> fn(x) { x }
      assert_equal(100, result)
    }),
    "pipe into capture"
    |> example(fn() {
      let result =
        1
        |> pair(2, _)
      assert_equal(#(2, 1), result)
    }),
    "pipe first"
    |> example(fn() {
      let result =
        1
        |> pair(2)
      assert_equal(#(1, 2), result)
    }),
    "pipe middle with label requires no capture"
    |> example(fn() {
      let result =
        2
        |> triplet(z: 3, x: 1)
      assert_equal(#(1, 2, 3), result)
    }),
    "pipe last with label requires no capture"
    |> example(fn() {
      let result =
        3
        |> triplet(y: 2, x: 1)
      assert_equal(#(1, 2, 3), result)
    }),
  ]
}

fn tail_call_optimisation_tests(_fns) -> List(Test) {
  [
    "10 million recursions doesn't overflow the stack"
    |> example(fn() { assert_equal(Nil, count_down(from: 10_000_000)) }),
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
      "No fields assert assignment",
      fn() { assert importable.NoFields = importable.NoFields },
      importable.NoFields,
    ),
    lazy_equality_test(
      "No fields unqualified assert assignment",
      fn() { assert NoFields = importable.NoFields },
      importable.NoFields,
    ),
    lazy_equality_test(
      "No fields let assignment",
      fn() { let importable.NoFields = importable.NoFields },
      importable.NoFields,
    ),
    lazy_equality_test(
      "No fields unqualified let assignment",
      fn() { let NoFields = importable.NoFields },
      importable.NoFields,
    ),
  ]
}

fn equality_test(name: String, left: a, right: a) {
  example(name, fn() { assert_equal(left, right) })
}

fn lazy_equality_test(name: String, left: fn() -> a, right: a) {
  example(name, fn() { assert_equal(left(), right) })
}

fn try_fn(result) {
  try x = result
  Ok(x + 1)
}

fn try_tests(_fns) -> List(Test) {
  [
    "ok"
    |> example(fn() { assert_equal(Ok(2), try_fn(Ok(1))) }),
    "error"
    |> example(fn() { assert_equal(Error("error"), try_fn(Error("error"))) }),
    "ok in block"
    |> example(fn() {
      assert_equal(
        Ok(2),
        {
          try x = Ok(1)
          Ok(x + 1)
        },
      )
    }),
    "error in block"
    |> example(fn() {
      assert_equal(
        Error(Nil),
        {
          try x = Error(Nil)
          Ok(x + 1)
        },
      )
    }),
  ]
}

fn clause_guard_tests(_fns) -> List(Test) {
  let yes = True
  let no = False
  [
    "var True"
    |> example(fn() { assert_equal(0, case Nil {
          _ if yes -> 0
          _ -> 1
        }) }),
    "var False"
    |> example(fn() { assert_equal(1, case Nil {
          _ if no -> 0
          _ -> 1
        }) }),
  ]
  // TODO
  // ClauseGuard::Equals { left, right, .. } => {
  // ClauseGuard::NotEquals { left, right, .. } => {
  // ClauseGuard::GtFloat { left, right, .. } 
  // ClauseGuard::GtInt { left, right, .. } => {
  // ClauseGuard::GtEqFloat { left, right, .. }
  // ClauseGuard::GtEqInt { left, right, .. } => {
  // ClauseGuard::LtFloat { left, right, .. } | 
  // ClauseGuard::LtInt { left, right, .. } => {
  // ClauseGuard::LtEqFloat { left, right, .. }
  // ClauseGuard::LtEqInt { left, right, .. } => {
  // ClauseGuard::Or { left, right, .. } => {
  // ClauseGuard::And { left, right, .. } => {
  // ClauseGuard::TupleIndex { .. } => 
  // ClauseGuard::Constant(_) => 
  // nested operators to check precedence
}
