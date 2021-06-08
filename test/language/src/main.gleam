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
      suite("ints", int_tests(fns)),
      suite("floats", float_tests(fns)),
      suite("pipes", pipes_tests(fns)),
      suite("strings", strings_tests(fns)),
      suite("constants", constants_tests(fns)),
      suite("clause guards", clause_guard_tests(fns)),
      suite("imported custom types", imported_custom_types_test(fns)),
      suite("tail call optimisation", tail_call_optimisation_tests(fns)),
      suite("alternative patterns", alternative_patterns_tests(fns)),
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

fn strings_tests(_fns) -> List(Test) {
  [equality_test("Empty", "", ""), equality_test("Newlines", "
", "\n")]
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
    "pipe middle with label requires false capture"
    |> example(fn() {
      let result =
        2
        |> triplet(z: 3, x: 1)
      assert_equal(#(1, 2, 3), result)
    }),
    "pipe last with label requires false capture"
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

fn true() {
  True
}

fn false() {
  False
}

fn make_int_zero() {
  0
}

fn make_float_zero() {
  0.0
}

fn make_pair(a, b) {
  #(a, b)
}

fn make_ok(value) {
  Ok(value)
}

fn make_error(reason) {
  Error(reason)
}

// Constructor functions are used rather than literals to stop the Erlang
// compiler being clever and complaining about the guards always having the
// same result
fn clause_guard_tests(_fns) -> List(Test) {
  // Testing that the name reuse is valid
  let true = true()
  // Testing that the name reuse is valid
  let false = false()
  let int_zero = make_int_zero()
  let int_one = make_int_zero() + 1
  let float_zero = make_float_zero()
  let float_one = make_float_zero() +. 1.0
  let tuple_true_false = make_pair(True, False)
  let ok = make_ok(1)
  let error = make_error(1)

  [
    "var True"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if true -> 0
          _ -> 1
        },
      )
    }),
    "var False"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false -> 0
          _ -> 1
        },
      )
    }),
    "int equals match"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_zero == int_zero -> 0
          _ -> 1
        },
      )
    }),
    "int equals nomatch"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if int_zero == int_one -> 0
          _ -> 1
        },
      )
    }),
    "int not equals match"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_zero != int_one -> 0
          _ -> 1
        },
      )
    }),
    "int not equals nomatch"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if int_zero != int_zero -> 0
          _ -> 1
        },
      )
    }),
    "record equals match"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if ok == ok -> 0
          _ -> 1
        },
      )
    }),
    "record equals nomatch"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if ok == error -> 0
          _ -> 1
        },
      )
    }),
    "record not equals match"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if ok != error -> 0
          _ -> 1
        },
      )
    }),
    "record not equals nomatch"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if error != error -> 0
          _ -> 1
        },
      )
    }),
    "and true true"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if true && true -> 0
          _ -> 1
        },
      )
    }),
    "and true false"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if true && false -> 0
          _ -> 1
        },
      )
    }),
    "and false true"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false && true -> 0
          _ -> 1
        },
      )
    }),
    "and false false"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false && false -> 0
          _ -> 1
        },
      )
    }),
    "or true true"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if true || true -> 0
          _ -> 1
        },
      )
    }),
    "or true false"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if true || false -> 0
          _ -> 1
        },
      )
    }),
    "or false true"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if false || true -> 0
          _ -> 1
        },
      )
    }),
    "or false false"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false || false -> 0
          _ -> 1
        },
      )
    }),
    "1. >. 0."
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if float_one >. float_zero -> 0
          _ -> 1
        },
      )
    }),
    "0. >. 0."
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if float_zero >. float_zero -> 0
          _ -> 1
        },
      )
    }),
    "1. >=. 0."
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if float_one >=. float_zero -> 0
          _ -> 1
        },
      )
    }),
    "0. >=. 0."
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if float_zero >=. float_zero -> 0
          _ -> 1
        },
      )
    }),
    "0. >=. 1."
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if float_zero >=. float_one -> 0
          _ -> 1
        },
      )
    }),
    "0. <. 1."
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if float_zero <. float_one -> 0
          _ -> 1
        },
      )
    }),
    "0. <. 0."
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if float_zero <. float_zero -> 0
          _ -> 1
        },
      )
    }),
    "0. <=. 1."
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if float_zero <=. float_one -> 0
          _ -> 1
        },
      )
    }),
    "0. <=. 0."
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if float_zero <=. float_zero -> 0
          _ -> 1
        },
      )
    }),
    "1. <=. 0."
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if float_one <=. float_zero -> 0
          _ -> 1
        },
      )
    }),
    "1 > 0"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_one > int_zero -> 0
          _ -> 1
        },
      )
    }),
    "0 > 0"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if int_zero > int_zero -> 0
          _ -> 1
        },
      )
    }),
    "1 >= 0"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_one >= int_zero -> 0
          _ -> 1
        },
      )
    }),
    "0 >= 0"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_zero >= int_zero -> 0
          _ -> 1
        },
      )
    }),
    "0 >= 1"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if int_zero >= int_one -> 0
          _ -> 1
        },
      )
    }),
    "0 < 1"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_zero < int_one -> 0
          _ -> 1
        },
      )
    }),
    "0 < 0"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if int_zero < int_zero -> 0
          _ -> 1
        },
      )
    }),
    "0 <= 1"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_zero <= int_one -> 0
          _ -> 1
        },
      )
    }),
    "0 <= 0"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_zero <= int_zero -> 0
          _ -> 1
        },
      )
    }),
    "1 <= 0"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if int_one <= int_zero -> 0
          _ -> 1
        },
      )
    }),
    "#(True, False).0"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if tuple_true_false.0 -> 0
          _ -> 1
        },
      )
    }),
    "#(True, False).1"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if tuple_true_false.1 -> 0
          _ -> 1
        },
      )
    }),
    "const 0"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if int_zero == 0 -> 0
          _ -> 1
        },
      )
    }),
    "const 1"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if int_zero == 1 -> 0
          _ -> 1
        },
      )
    }),
    "const Ok(1)"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if ok == Ok(1) -> 0
          _ -> 1
        },
      )
    }),
    "const Error(1)"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if ok == Error(1) -> 0
          _ -> 1
        },
      )
    }),
  ]
  // TODO
  // nested operators to check precedence
}

fn alternative_patterns_tests(_fns) -> List(Test) {
  [
    "numbers"
    |> example(fn() {
      assert_equal(
        0,
        case 4 {
          1 | 2 | 3 | 4 -> 0
          _ -> 1
        },
      )
    }),
    "lists"
    |> example(fn() {
      assert_equal(
        0,
        case [1, 2] {
          [0] | [1, 2] -> 0
          _ -> 1
        },
      )
    }),
    "assignment"
    |> example(fn() {
      assert_equal(
        2,
        case [1, 2] {
          [x] | [_, x] -> x
          _ -> 0
        },
      )
    }),
    "multiple assignment"
    |> example(fn() {
      assert_equal(
        #(1, 2),
        case [1, 2, 3] {
          [x, y] | [x, y, 3] -> #(x, y)
          _ -> #(0, 0)
        },
      )
    }),
    "guard"
    |> example(fn() {
      assert_equal(
        2,
        case [1, 2] {
          [x] | [_, x] if x == 2 -> x
          _ -> 0
        },
      )
    }),
    "guard left-hand side"
    |> example(fn() {
      assert_equal(
        1,
        case [1] {
          [x] | [_, x] if x == 1 -> x
          _ -> 0
        },
      )
    }),
  ]
}
