//// Here are some things that have been previously been incorrectly reported as
//// unused.

import test.{Test, assert_equal, example, operator_test, suite}
import importable.{NoFields}
import mod_with_numbers_0123456789
import record_update
import shadowed_module.{ShadowPerson}
import gleam
import port.{Port}

pub fn main() -> Int {
  let stats =
    test.run([
      suite("try", try_tests()),
      suite("ints", int_tests()),
      suite("pipes", pipes_tests()),
      suite("assert", assert_tests()),
      suite("floats", float_tests()),
      suite("prelude", prelude_tests()),
      suite("strings", strings_tests()),
      suite("equality", equality_tests()),
      suite("constants", constants_tests()),
      suite("bit strings", bit_string_tests()),
      suite("sized bit strings", sized_bit_string_tests()),
      suite("list spread", list_spread_tests()),
      suite("clause guards", clause_guard_tests()),
      suite("imported custom types", imported_custom_types_test()),
      suite("tail call optimisation", tail_call_optimisation_tests()),
      suite("alternative patterns", alternative_patterns_tests()),
      suite("multiple case subjects", multiple_case_subjects()),
      suite("precedence", precedence_tests()),
      suite("call returned function", call_returned_function_tests()),
      suite("floats", floats_tests()),
      suite("ints", ints_tests()),
      suite("mod with numbers", mod_with_numbers_tests()),
      suite("record update", record_update_tests()),
      suite("record access", record_access_tests()),
      suite("shadowed module", shadowed_module_tests()),
      suite("unicode overflow", unicode_overflow_tests()),
      suite("negation", negation_tests()),
      suite("bit string match", bit_string_match_tests()),
    ])

  case stats.failures {
    0 -> 0
    _ -> 1
  }
}

fn int_tests() -> List(Test) {
  let basic_addition = operator_test("+", fn(a, b) { a + b })
  let basic_subtraction = operator_test("-", fn(a, b) { a - b })
  let basic_multiplication = operator_test("*", fn(a, b) { a * b })
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

fn float_tests() -> List(Test) {
  let basic_addition = operator_test("+.", fn(a, b) { a +. b })
  let basic_subtraction = operator_test("-.", fn(a, b) { a -. b })
  let basic_multiplication = operator_test("*.", fn(a, b) { a *. b })
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

fn strings_tests() -> List(Test) {
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

fn pipes_tests() -> List(Test) {
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

fn assert_tests() -> List(Test) {
  [
    "assert Ok(_)"
    |> example(fn() {
      assert_equal(
        Ok(1),
        {
          assert Ok(_) = Ok(1)
        },
      )
    }),
    "assert Ok(x)"
    |> example(fn() {
      assert_equal(
        1,
        {
          assert Ok(x) = Ok(1)
          x
        },
      )
    }),
  ]
}

fn tail_call_optimisation_tests() -> List(Test) {
  [
    "10 million recursions doesn't overflow the stack"
    |> example(fn() { assert_equal(Nil, count_down(from: 10_000_000)) }),
    // https://github.com/gleam-lang/gleam/issues/1214
    // https://github.com/gleam-lang/gleam/issues/1380
    "Arguments correctly reassigned"
    |> example(fn() {
      assert_equal([1, 2, 3], tail_recursive_accumulate_down(3, []))
    }),
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

fn constants_tests() -> List(Test) {
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

fn imported_custom_types_test() -> List(Test) {
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

fn try_tests() -> List(Test) {
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
fn clause_guard_tests() -> List(Test) {
  // Testing that the name reuse is valid
  let true_ = true()
  // Testing that the name reuse is valid
  let false_ = false()
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
          _ if true_ -> 0
          _ -> 1
        },
      )
    }),
    "var False"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false_ -> 0
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
          _ if true_ && true_ -> 0
          _ -> 1
        },
      )
    }),
    "and true false"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if true_ && false_ -> 0
          _ -> 1
        },
      )
    }),
    "and false true"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false_ && true_ -> 0
          _ -> 1
        },
      )
    }),
    "and false false"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false_ && false_ -> 0
          _ -> 1
        },
      )
    }),
    "or true true"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if true_ || true_ -> 0
          _ -> 1
        },
      )
    }),
    "or true false"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if true_ || false_ -> 0
          _ -> 1
        },
      )
    }),
    "or false true"
    |> example(fn() {
      assert_equal(
        0,
        case Nil {
          _ if false_ || true_ -> 0
          _ -> 1
        },
      )
    }),
    "or false false"
    |> example(fn() {
      assert_equal(
        1,
        case Nil {
          _ if false_ || false_ -> 0
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

fn prelude_tests() -> List(Test) {
  [
    "gleam.Ok"
    |> example(fn() { assert_equal(Ok(1), gleam.Ok(1)) }),
    "gleam.Error"
    |> example(fn() { assert_equal(Error(1), gleam.Error(1)) }),
    "gleam.Nil"
    |> example(fn() { assert_equal(Nil, gleam.Nil) }),
  ]
}

fn alternative_patterns_tests() -> List(Test) {
  let int_one = make_int_zero() + 1
  let int_two = make_int_zero() + 2

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
          [x] | [_, x] if x == int_two -> x
          _ -> 0
        },
      )
    }),
    "guard left-hand side"
    |> example(fn() {
      assert_equal(
        1,
        case [1] {
          [x] | [_, x] if x == int_one -> x
          _ -> 0
        },
      )
    }),
  ]
}

fn multiple_case_subjects() -> List(Test) {
  [
    "wildcard"
    |> example(fn() {
      assert_equal(
        0,
        case True, False {
          _, _ -> 0
        },
      )
    }),
    "no match"
    |> example(fn() {
      assert_equal(
        0,
        case True, False {
          False, True -> 1
          _, _ -> 0
        },
      )
    }),
    "match"
    |> example(fn() {
      assert_equal(
        0,
        case True, False {
          False, True -> 1
          _, _ -> 0
        },
      )
    }),
    "alternative"
    |> example(fn() {
      assert_equal(
        1,
        case True, False {
          False, True | True, False -> 1
          _, _ -> 0
        },
      )
    }),
    "guard"
    |> example(fn() {
      assert_equal(
        1,
        case True, True {
          x, y if x == y -> 1
          _, _ -> 0
        },
      )
    }),
  ]
}

fn equality_tests() -> List(Test) {
  [
    "[] == []"
    |> example(fn() { assert_equal(True, [] == []) }),
    "[] == [0]"
    |> example(fn() { assert_equal(False, [] == [0]) }),
    "[0] == []"
    |> example(fn() { assert_equal(False, [0] == []) }),
    "[0] == [0]"
    |> example(fn() { assert_equal(True, [0] == [0]) }),
    "[] != []"
    |> example(fn() { assert_equal(False, [] != []) }),
    "[] != [0]"
    |> example(fn() { assert_equal(True, [] != [0]) }),
    "[0] != []"
    |> example(fn() { assert_equal(True, [0] != []) }),
    "[0] != [0]"
    |> example(fn() { assert_equal(False, [0] != [0]) }),
    "0 == 0"
    |> example(fn() { assert_equal(True, 0 == 0) }),
    "0 != 0"
    |> example(fn() { assert_equal(False, 0 != 0) }),
    "1 == 0"
    |> example(fn() { assert_equal(False, 1 == 0) }),
    "1 != 0"
    |> example(fn() { assert_equal(True, 1 != 0) }),
    "1 == 1"
    |> example(fn() { assert_equal(True, 1 == 1) }),
    "1 != 1"
    |> example(fn() { assert_equal(False, 1 != 1) }),
    "<<>> == <<>>"
    |> example(fn() { assert_equal(True, <<>> == <<>>) }),
    "<<>> != <<>>"
    |> example(fn() { assert_equal(False, <<>> != <<>>) }),
    // Bit strings
    "<<1, 2>> == <<1, 2>>"
    |> example(fn() { assert_equal(True, <<1, 2>> == <<1, 2>>) }),
    "<<1, 2>> != <<1, 2>>"
    |> example(fn() { assert_equal(False, <<1, 2>> != <<1, 2>>) }),
    "<<1, 2>> == <<2>>"
    |> example(fn() { assert_equal(False, <<1, 2>> == <<2>>) }),
    "<<1, 2>> != <<2>>"
    |> example(fn() { assert_equal(True, <<1, 2>> != <<2>>) }),
    // Records
    "Ok(1) == Ok(1)"
    |> example(fn() { assert_equal(True, Ok(1) == Ok(1)) }),
    "Ok(1) != Ok(1)"
    |> example(fn() { assert_equal(False, Ok(1) != Ok(1)) }),
    "Ok(2) == Ok(1)"
    |> example(fn() { assert_equal(False, Ok(2) == Ok(1)) }),
    "Ok(2) != Ok(1)"
    |> example(fn() { assert_equal(True, Ok(2) != Ok(1)) }),
    "Error(1) == Error(1)"
    |> example(fn() { assert_equal(True, Error(1) == Error(1)) }),
    "Error(1) != Error(1)"
    |> example(fn() { assert_equal(False, Error(1) != Error(1)) }),
    "Error(2) == Error(1)"
    |> example(fn() { assert_equal(False, Error(2) == Error(1)) }),
    "Error(2) != Error(1)"
    |> example(fn() { assert_equal(True, Error(2) != Error(1)) }),
  ]
}

fn bit_string_tests() -> List(Test) {
  [
    "<<\"Gleam\":utf8, \"👍\":utf8>> == <<\"Gleam\":utf8, \"👍\":utf8>>"
    |> example(fn() {
      assert_equal(
        True,
        <<"Gleam":utf8, "👍":utf8>> == <<"Gleam":utf8, "👍":utf8>>,
      )
    }),
    "<<\"Gleam\":utf8, \"👍\":utf8>> == <<\"👍\":utf8>>"
    |> example(fn() {
      assert_equal(False, <<"Gleam":utf8, "👍":utf8>> == <<"👍":utf8>>)
    }),
    "<<\"abc\":utf8>> == <<97, 98, 99>>"
    |> example(fn() { assert_equal(True, <<"abc":utf8>> == <<97, 98, 99>>) }),
    "<<<<1>>:bit_string, 2>> == <<1, 2>>"
    |> example(fn() { assert_equal(True, <<<<1>>:bit_string, 2>> == <<1, 2>>) }),
    "<<1>> == <<1:int>>"
    |> example(fn() { assert_equal(True, <<1>> == <<1:int>>) }),
    "<<1>> == <<1.0:float>>"
    |> example(fn() { assert_equal(True, <<63,240,0,0,0,0,0,0>> == <<1.0:float>>) }),
  ]
}

fn sized_bit_string_tests() -> List(Test) {
  [
    "<<1>> == <<257:size(8)>>"
    |> example(fn() {
      assert_equal(True, <<1>> ==<<257:size(8)>>)
    }),
    "<<1, 1>> == <<257:size(16)>>"
    |> example(fn() {
      assert_equal(True, <<1, 1>> ==<<257:size(16)>>)
    }),
    "<<1, 1>> == <<257:size(24)>>"
    |> example(fn() {
      assert_equal(True, <<0, 1, 1>> ==<<257:size(24)>>)
    }),
    "<<1, 0, 0, 0, 1>> == <<4294967297:size(40)>>"
    |> example(fn() {
      assert_equal(True, <<1, 0, 0, 0, 1>> ==<<4294967297:size(40)>>)
    }),
    "<<>> == <<256:size(-1)>>"
    |> example(fn() {
      assert_equal(True, <<>> ==<<256:size(-1)>>)
    }),
    // JS Number.MAX_SAFE_INTEGER
    "<<0, 31, 255, 255, 255, 255, 255, 255>> == <<9007199254740991:size(64)>>"
    |> example(fn() {
      assert_equal(True, <<0, 31, 255, 255, 255, 255, 255, 255>> ==<<9007199254740991:size(64)>>)
    }),
  ]
}

fn list_spread_tests() -> List(Test) {
  [
    "[1, ..[]]"
    |> example(fn() { assert_equal([1, ..[]], [1]) }),
    "[1, 2, ..[]]"
    |> example(fn() { assert_equal([1, 2, ..[]], [1, 2]) }),
    "[1, 2, ..[3]]"
    |> example(fn() { assert_equal([1, 2, ..[3]], [1, 2, 3]) }),
    "[1, 2, ..[3, 4]]"
    |> example(fn() { assert_equal([1, 2, ..[3, 4]], [1, 2, 3, 4]) }),
  ]
}

fn precedence_tests() -> List(Test) {
  [
    "1 + 2 * 3"
    |> example(fn() { assert_equal(7, 1 + 2 * 3) }),
    "3 * 1 + 2"
    |> example(fn() { assert_equal(5, 3 * 1 + 2) }),
    "{ 1 + 2 } * 3"
    |> example(fn() { assert_equal(9, { 1 + 2 } * 3) }),
    "3 * { 1 + 2 }"
    |> example(fn() { assert_equal(9, 3 * { 1 + 2 }) }),
    "1 + 2 * 3 + 4"
    |> example(fn() { assert_equal(11, 1 + 2 * 3 + 4) }),
    "2 * 3 + 4 * 5"
    |> example(fn() { assert_equal(26, 2 * 3 + 4 * 5) }),
    "2 * { 3 + 1 } / 2"
    |> example(fn() { assert_equal(4, 2 * { 3 + 1 } / 2) }),
    "5 + 3 / 3 * 2 - 6 * 4"
    |> example(fn() { assert_equal(-17, 5 + 3 / 3 * 2 - 6 * 4) }),
  ]
}

type FnBox {
  FnBox(f: fn(Int) -> Int)
}

fn call_returned_function_tests() -> List(Test) {
  [
    "call record access"
    |> example(fn() {
      let b = FnBox(f: fn(x) { x })
      assert_equal(5, b.f(5))
    }),
    "call tuple access"
    |> example(fn() {
      let t = #(fn(x) { x })
      assert_equal(5, t.0(5))
    }),
  ]
}

fn floats_tests() -> List(Test) {
  [
    "2.0 /. 2.0"
    |> example(fn() { assert_equal(1.0, 2.0 /. 2.0) }),
    "2.0 /. 0.0"
    |> example(fn() { assert_equal(0.0, 2.0 /. 0.0) }),
  ]
}

fn ints_tests() -> List(Test) {
  [
    "hex int"
    |> example(fn() { assert_equal(15, 0xF) }),
    "octal int"
    |> example(fn() { assert_equal(15, 0o17) }),
    "binary int"
    |> example(fn() { assert_equal(15, 0b00001111) }),
    "1-1 should lex as 1 - 1"
    |> example(fn() { assert_equal(0, 1 - 1) }),
    "a-1 should lex as a - 1"
    |> example(fn() {
      let a = 1
      assert_equal(0, a - 1)
    }),
    "1- 1 should lex as 1 - 1"
    |> example(fn() { assert_equal(0, 1 - 1) }),
    "1 / 1"
    |> example(fn() { assert_equal(1, 1 / 1) }),
    "1 / 0"
    |> example(fn() { assert_equal(0, 1 / 0) }),
    "3 / 2"
    |> example(fn() { assert_equal(1, 3 / 2) }),
    "3 / 0"
    |> example(fn() { assert_equal(0, 3 / 0) }),
  ]
}

fn mod_with_numbers_tests() -> List(Test) {
  [
    "mod_with_numbers_0123456789.hello()"
    |> example(fn() {
      assert_equal("world", mod_with_numbers_0123456789.hello())
    }),
  ]
}

type Person {
  Person(name: String, age: Int, country: String)
}

fn record_update_tests() {
  [
    "unqualified record update"
    |> example(fn() {
      let past = Person("Quinn", 27, "Canada")
      let present = Person(..past, country: "USA", age: past.age + 1)
      assert_equal(Person("Quinn", 28, "USA"), present)
    }),
    "qualified record update"
    |> example(fn() {
      let module_box = record_update.Box("a", 5)
      let updated = record_update.Box(..module_box, value: 6)
      assert_equal(record_update.Box("a", 6), updated)
    }),
    // https://github.com/gleam-lang/gleam/issues/1379
    "pipe in record update"
    |> example(fn() {
      let module_box = record_update.Box("a", 5)
      let updated =
        record_update.Box(
          ..module_box,
          value: 6
          |> id,
        )
      assert_equal(record_update.Box("a", 6), updated)
    }),
  ]
}

fn record_access_tests() {
  let person = Person(name: "Quinn", age: 27, country: "Canada")
  [
    "record access 1"
    |> example(fn() { assert_equal(person.name, "Quinn") }),
    "record access 2"
    |> example(fn() { assert_equal(person.age, 27) }),
    // https://github.com/gleam-lang/gleam/issues/1093
    "contextual info for access"
    |> example(fn() {
      let apply = fn(a, f) { f(a) }
      assert_equal(apply(person, fn(x) { x.name }), "Quinn")
    }),
  ]
}

fn shadowed_module_tests() {
  [
    "this module"
    |> example(fn() {
      let shadowed_module = ShadowPerson(18)
      let shadowed_module = shadowed_module.celebrate_birthday(shadowed_module)
      assert_equal(19, shadowed_module.age)
    }),
  ]
}

fn unicode_overflow_tests() {
  // In erlang, literally creating binaries can cause entries to overflow.
  // For example `<<"🌵">> == <<"5">>` evaluates to true.
  // This checks that we are not doing that.
  // See: https://github.com/gleam-lang/gleam/issues/457
  [
    "🌵 vs 5"
    |> example(fn() { assert_equal(False, "🌵" == "5") }),
  ]
}

type PortMonitorFlag {
  Port
}

pub external fn go(Port) -> Nil =
  "" ""

fn id(x) {
  x
}

// https://github.com/gleam-lang/gleam/issues/1214
// https://github.com/gleam-lang/gleam/issues/1380
fn tail_recursive_accumulate_down(x, y) {
  case x {
    0 -> y
    _ -> tail_recursive_accumulate_down(x - 1, [x, ..y])
  }
}

fn negation_tests() {
  [
    "!True"
    |> example(fn() { assert_equal(False, !True) }),
    "!False"
    |> example(fn() { assert_equal(True, !False) }),
    "!!False"
    |> example(fn() { assert_equal(False, !!False) }),
    "!!True"
    |> example(fn() { assert_equal(True, !!True) }),
    // This would crash if the right hand side evaluated
    "!True && assert False = True"
    |> example(fn() { assert_equal(False, !True && assert False = True) }),
    "!False || assert False = True"
    |> example(fn() { assert_equal(True, !False || assert False = True) }),
  ]
}

fn bit_string_match_tests() {
  [
    "let <<1, x>> = <<1, 2>>"
    |> example(fn() { assert_equal(2, {
      let <<1, x>> = <<1, 2>>
      x
    }) }),
    "let <<a:8>> = <<1>>"
    |> example(fn() { assert_equal(1, 
      {
        let <<a:8>> = <<1>>
        a
      }) 
    }),
    "let <<a:16, b:8>> = <<1, 2, 3>>"
    |> example(fn() { assert_equal(#(258, 3), 
      {
        let <<a:16, b:8>> = <<1, 2, 3>>
        #(a, b)
      }) 
    }),
    "let <<a:float, b:int>> = <<63,240,0,0,0,0,0,0,1>>"
    |> example(fn() { assert_equal(#(1.0, 1), 
      {
        let <<a:float, b:int>> = <<63,240,0,0,0,0,0,0,1>>
        #(a, b)
      }) 
    }),
    "let <<a:float>> = <<1.23:float>>"
    |> example(fn() { assert_equal(1.23, 
      {
        let <<a:float>> = <<1.23:float>>
        a
      }) 
    }),
    "let <<_, rest:binary>> = <<1>>"
    |> example(fn() { assert_equal(<<>>,
      {
        let <<_, rest:binary>> = <<1>>
        rest
      })
    }),
        "let <<_, rest:binary>> = <<1,2,3>>"
    |> example(fn() { assert_equal(<<2,3>>,
      {
        let <<_, rest:binary>> = <<1,2,3>>
        rest
      })
    }),
  ]
}
