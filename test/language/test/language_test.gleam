//// Here are some things that have been previously been incorrectly reported as
//// unused.

import tests.{type Test, assert_equal, example, operator_test, suite}
import importable.{NoFields}
import mod_with_numbers_0123456789
import record_update
import shadowed_module.{ShadowPerson}
import ffi.{file_exists}
import gleam

pub fn main() {
  let stats =
    tests.run([
      suite("ints", int_tests()),
      suite("pipes", pipes_tests()),
      suite("blocks", block_tests()),
      suite("assert", assert_tests()),
      suite("floats", float_tests()),
      suite("prelude", prelude_tests()),
      suite("strings", strings_tests()),
      suite("equality", equality_tests()),
      suite("constants", constants_tests()),
      suite("bit strings target", bit_array_target_tests()),
      suite("bit strings", bit_array_tests()),
      suite("sized bit strings", sized_bit_array_tests()),
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
      suite("remainder", remainder_tests()),
      suite("mod with numbers", mod_with_numbers_tests()),
      suite("record update", record_update_tests()),
      suite("record access", record_access_tests()),
      suite("shadowed module", shadowed_module_tests()),
      suite("unicode overflow", unicode_overflow_tests()),
      suite("bool negation", bool_negation_tests()),
      suite("number negation", int_negation_tests()),
      suite("bit string match", bit_array_match_tests()),
      suite("anonymous functions", anonymous_function_tests()),
      suite("string pattern matching", string_pattern_matching_tests()),
      suite("typescript file inclusion", typescript_file_included_tests()),
      suite("custom types mixed args match", mixed_arg_match_tests()),
      suite("tuple access", tuple_access_tests()),
    ])

  ffi.halt(case stats.failures {
    0 -> 0
    _ -> 1
  })
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
    basic_addition(0.0, 0.0, 0.0),
    basic_addition(1.0, 1.0, 2.0),
    basic_addition(5.0, 1.0, 6.0),
    basic_addition(1.0, 3.0, 4.0),
    basic_addition(1.0, -3.0, -2.0),
    basic_subtraction(0.0, 0.0, 0.0),
    basic_subtraction(1.0, 1.0, 0.0),
    basic_subtraction(5.0, 1.0, 4.0),
    basic_subtraction(1.0, 3.0, -2.0),
    basic_subtraction(1.0, -3.0, 4.0),
    basic_subtraction(0.5, 0.0, 0.5),
    basic_subtraction(1.0, 4.5, -3.5),
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
    // scientific notation
    basic_addition(0.0e0, 0.0, 0.0),
    basic_addition(0.0, 0.0e0, 0.0),
    basic_addition(0.0e-0, 0.0, 0.0),
    basic_addition(0.0, 0.0e-0, 0.0),
    basic_addition(1.0e3, 1.0, 1001.0),
    basic_addition(5.0, 1.0e3, 1005.0),
    basic_addition(1.0e5, -3.0e5, -200_000.0),
    basic_addition(1.0e50, -1.0e50, 0.0),
    basic_addition(1.0e-3, 1.0, 1.001),
    basic_addition(5.0, 1.0e-3, 5.001),
    basic_addition(10.0e-2, -3.0e-2, 0.07),
    basic_subtraction(0.0e0, 0.0, 0.0),
    basic_subtraction(0.0, 0.0e0, 0.0),
    basic_subtraction(0.0e-0, 0.0, 0.0),
    basic_subtraction(0.0, 0.0e-0, 0.0),
    basic_subtraction(1.0e3, 1.0, 999.0),
    basic_subtraction(5.0, 1.0e3, -995.0),
    basic_subtraction(1.0e5, 1.0e5, 0.0),
    basic_subtraction(1.0e-3, 1.0, -0.999),
    basic_subtraction(5.0, 1.0e-3, 4.999),
    basic_subtraction(10.0e-2, -3.0e-2, 0.13),
    basic_multiplication(0.0e0, 0.0, 0.0),
    basic_multiplication(0.0, 0.0e0, 0.0),
    basic_multiplication(0.0e-0, 0.0, 0.0),
    basic_multiplication(0.0, 0.0e-0, 0.0),
    basic_multiplication(2.0e1, 2.0e1, 400.0),
    basic_multiplication(1.0e-5, 1.0e5, 1.0),
    basic_multiplication(2.0e5, 2.0e-5, 4.0),
    basic_multiplication(2.0e5, 4.0e-4, 80.0),
    basic_multiplication(2.0e-5, 2.0e5, 4.0),
    basic_multiplication(2.0e5, 4.0e-5, 8.0),
    basic_multiplication(-2.0e-5, 2.0e5, -4.0),
    basic_multiplication(-2.0e5, -4.0e-5, 8.0),
  ]
}

fn strings_tests() -> List(Test) {
  [
    equality_test("Empty", "", ""),
    equality_test(
      "Newlines",
      "
",
      "\n",
    ),
    "let assert string prefix"
    |> example(fn() {
      let assert "ab" <> rest = "abcdef"
      assert_equal("cdef", rest)
    }),
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
    "let assert Ok(_)"
    |> example(fn() {
      assert_equal(Ok(1), {
        let assert Ok(_) = Ok(1)
      })
    }),
    "let assert Ok(x)"
    |> example(fn() {
      assert_equal(1, {
        let assert Ok(x) = Ok(1)
        x
      })
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
    // https://github.com/gleam-lang/gleam/issues/2400
    "not recursion, the function is shadowed its argument"
    |> example(fn() {
      assert_equal(function_shadowed_by_own_argument(fn() { 1 }), 1)
    }),
  ]
}

fn function_shadowed_by_own_argument(function_shadowed_by_own_argument) {
  function_shadowed_by_own_argument()
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
      fn() {
        let assert importable.NoFields = importable.NoFields
      },
      importable.NoFields,
    ),
    lazy_equality_test(
      "No fields unqualified assert assignment",
      fn() {
        let assert NoFields = importable.NoFields
      },
      importable.NoFields,
    ),
    lazy_equality_test(
      "No fields let assignment",
      fn() {
        let importable.NoFields = importable.NoFields
      },
      importable.NoFields,
    ),
    lazy_equality_test(
      "No fields unqualified let assignment",
      fn() {
        let NoFields = importable.NoFields
      },
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
      assert_equal(0, case Nil {
        _ if true_ -> 0
        _ -> 1
      })
    }),
    "var False"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if false_ -> 0
        _ -> 1
      })
    }),
    "int equals match"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_zero == int_zero -> 0
        _ -> 1
      })
    }),
    "int equals nomatch"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if int_zero == int_one -> 0
        _ -> 1
      })
    }),
    "int not equals match"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_zero != int_one -> 0
        _ -> 1
      })
    }),
    "int not equals nomatch"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if int_zero != int_zero -> 0
        _ -> 1
      })
    }),
    "record equals match"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if ok == ok -> 0
        _ -> 1
      })
    }),
    "record equals nomatch"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if ok == error -> 0
        _ -> 1
      })
    }),
    "record not equals match"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if ok != error -> 0
        _ -> 1
      })
    }),
    "record not equals nomatch"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if error != error -> 0
        _ -> 1
      })
    }),
    "and true true"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if true_ && true_ -> 0
        _ -> 1
      })
    }),
    "and true false"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if true_ && false_ -> 0
        _ -> 1
      })
    }),
    "and false true"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if false_ && true_ -> 0
        _ -> 1
      })
    }),
    "and false false"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if false_ && false_ -> 0
        _ -> 1
      })
    }),
    "or true true"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if true_ || true_ -> 0
        _ -> 1
      })
    }),
    "or true false"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if true_ || false_ -> 0
        _ -> 1
      })
    }),
    "or false true"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if false_ || true_ -> 0
        _ -> 1
      })
    }),
    "or false false"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if false_ || false_ -> 0
        _ -> 1
      })
    }),
    "1. >. 0."
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if float_one >. float_zero -> 0
        _ -> 1
      })
    }),
    "0. >. 0."
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if float_zero >. float_zero -> 0
        _ -> 1
      })
    }),
    "1. >=. 0."
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if float_one >=. float_zero -> 0
        _ -> 1
      })
    }),
    "0. >=. 0."
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if float_zero >=. float_zero -> 0
        _ -> 1
      })
    }),
    "0. >=. 1."
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if float_zero >=. float_one -> 0
        _ -> 1
      })
    }),
    "0. <. 1."
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if float_zero <. float_one -> 0
        _ -> 1
      })
    }),
    "0. <. 0."
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if float_zero <. float_zero -> 0
        _ -> 1
      })
    }),
    "0. <=. 1."
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if float_zero <=. float_one -> 0
        _ -> 1
      })
    }),
    "0. <=. 0."
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if float_zero <=. float_zero -> 0
        _ -> 1
      })
    }),
    "1. <=. 0."
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if float_one <=. float_zero -> 0
        _ -> 1
      })
    }),
    "1 > 0"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_one > int_zero -> 0
        _ -> 1
      })
    }),
    "0 > 0"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if int_zero > int_zero -> 0
        _ -> 1
      })
    }),
    "1 >= 0"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_one >= int_zero -> 0
        _ -> 1
      })
    }),
    "0 >= 0"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_zero >= int_zero -> 0
        _ -> 1
      })
    }),
    "0 >= 1"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if int_zero >= int_one -> 0
        _ -> 1
      })
    }),
    "0 < 1"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_zero < int_one -> 0
        _ -> 1
      })
    }),
    "0 < 0"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if int_zero < int_zero -> 0
        _ -> 1
      })
    }),
    "0 <= 1"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_zero <= int_one -> 0
        _ -> 1
      })
    }),
    "0 <= 0"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_zero <= int_zero -> 0
        _ -> 1
      })
    }),
    "1 <= 0"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if int_one <= int_zero -> 0
        _ -> 1
      })
    }),
    "#(True, False).0"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if tuple_true_false.0 -> 0
        _ -> 1
      })
    }),
    "#(True, False).1"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if tuple_true_false.1 -> 0
        _ -> 1
      })
    }),
    "const 0"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if int_zero == 0 -> 0
        _ -> 1
      })
    }),
    "const 1"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if int_zero == 1 -> 0
        _ -> 1
      })
    }),
    "const Ok(1)"
    |> example(fn() {
      assert_equal(0, case Nil {
        _ if ok == Ok(1) -> 0
        _ -> 1
      })
    }),
    "const Error(1)"
    |> example(fn() {
      assert_equal(1, case Nil {
        _ if ok == Error(1) -> 0
        _ -> 1
      })
    }),
    "tuple with pattern var"
    |> example(fn() {
      assert_equal(0, case True {
        a if #(a) == #(True) -> 0
        _ -> 1
      })
    }),
    "module access to string const(matches)"
    |> example(fn() {
      assert_equal(True, case "gleam" {
        lang if lang == importable.language -> True
        _ -> False
      })
    }),
    "module access to string cnost(does not match)"
    |> example(fn() {
      assert_equal(False, case "python" {
        lang if lang == importable.language -> True
        _ -> False
      })
    }),
    "module access to custom type const(matches)"
    |> example(fn() {
      assert_equal(True, case "WarGames" {
        movie if movie == importable.war_games.title -> True
        _ -> False
      })
    }),
    "module access to custom type const(does not match)"
    |> example(fn() {
      assert_equal(False, case "Gattaca" {
        movie if movie == importable.war_games.title -> True
        _ -> False
      })
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
      assert_equal(0, case 4 {
        1 | 2 | 3 | 4 -> 0
        _ -> 1
      })
    }),
    "lists"
    |> example(fn() {
      assert_equal(0, case [1, 2] {
        [0] | [1, 2] -> 0
        _ -> 1
      })
    }),
    "assignment"
    |> example(fn() {
      assert_equal(2, case [1, 2] {
        [x] | [_, x] -> x
        _ -> 0
      })
    }),
    "multiple assignment"
    |> example(fn() {
      assert_equal(#(1, 2), case [1, 2, 3] {
        [x, y] | [x, y, 3] -> #(x, y)
        _ -> #(0, 0)
      })
    }),
    "guard"
    |> example(fn() {
      assert_equal(2, case [1, 2] {
        [x] | [_, x] if x == int_two -> x
        _ -> 0
      })
    }),
    "guard left-hand side"
    |> example(fn() {
      assert_equal(1, case [1] {
        [x] | [_, x] if x == int_one -> x
        _ -> 0
      })
    }),
  ]
}

fn multiple_case_subjects() -> List(Test) {
  [
    "wildcard"
    |> example(fn() {
      assert_equal(0, case True, False {
        _, _ -> 0
      })
    }),
    "no match"
    |> example(fn() {
      assert_equal(0, case True, False {
        False, True -> 1
        _, _ -> 0
      })
    }),
    "match"
    |> example(fn() {
      assert_equal(0, case True, False {
        False, True -> 1
        _, _ -> 0
      })
    }),
    "alternative"
    |> example(fn() {
      assert_equal(1, case True, False {
        False, True | True, False -> 1
        _, _ -> 0
      })
    }),
    "guard"
    |> example(fn() {
      assert_equal(1, case True, True {
        x, y if x == y -> 1
        _, _ -> 0
      })
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
    // Bit arrays
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

fn bit_array_tests() -> List(Test) {
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
    "<<<<1>>:bit_array, 2>> == <<1, 2>>"
    |> example(fn() { assert_equal(True, <<<<1>>:bits, 2>> == <<1, 2>>) }),
    "<<1>> == <<1:int>>"
    |> example(fn() { assert_equal(True, <<1>> == <<1:int>>) }),
    "<<63, 240, 0, 0, 0, 0, 0, 0>> == <<1.0:float>>"
    |> example(fn() {
      assert_equal(True, <<63, 240, 0, 0, 0, 0, 0, 0>> == <<1.0:float>>)
    }),
  ]
}

@target(erlang)
fn bit_array_target_tests() -> List(Test) {
  [
    "<<60,0>> == <<1.0:float-size(16)>>"
    |> example(fn() { assert_equal(True, <<60, 0>> == <<1.0:float-16>>) }),
    "<<63,128,0,0>> == <<1.0:float-32>>"
    |> example(fn() {
      assert_equal(True, <<63, 128, 0, 0>> == <<1.0:float-32>>)
    }),
    "<<\"😀\":utf8>> == <<\"\u{1F600}\":utf8>>"
    |> example(fn() { assert_equal(True, <<"😀":utf8>> == <<"\u{1F600}":utf8>>) }),
  ]
}

@target(javascript)
fn bit_array_target_tests() -> List(Test) {
  []
}

fn sized_bit_array_tests() -> List(Test) {
  [
    "<<1>> == <<257:size(8)>>"
    |> example(fn() { assert_equal(True, <<1>> == <<257:size(8)>>) }),
    "<<1, 1>> == <<257:size(16)>>"
    |> example(fn() { assert_equal(True, <<1, 1>> == <<257:size(16)>>) }),
    "<<1, 1>> == <<257:size(24)>>"
    |> example(fn() { assert_equal(True, <<0, 1, 1>> == <<257:size(24)>>) }),
    "<<1, 0, 0, 0, 1>> == <<4294967297:size(40)>>"
    |> example(fn() {
      assert_equal(True, <<1, 0, 0, 0, 1>> == <<4_294_967_297:size(40)>>)
    }),
    "<<>> == <<256:size(-1)>>"
    |> example(fn() { assert_equal(True, <<>> == <<256:size(-1)>>) }),
    // JS Number.MAX_SAFE_INTEGER
    "<<0, 31, 255, 255, 255, 255, 255, 255>> == <<9007199254740991:size(64)>>"
    |> example(fn() {
      assert_equal(
        True,
        <<0, 31, 255, 255, 255, 255, 255, 255>> == <<
          9_007_199_254_740_991:size(64),
        >>,
      )
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
    "-5 + -3 / -3 * -2 - -6 * -4"
    |> example(fn() { assert_equal(-31, -5 + -3 / -3 * -2 - -6 * -4) }),
    "a + b / c * d - e * f"
    |> example(fn() {
      let a = 5
      let b = 3
      let c = 3
      let d = 2
      let e = 6
      let f = 4
      assert_equal(-17, a + b / c * d - e * f)
    }),
    "-a + -b / -c * -d - -e * -f"
    |> example(fn() {
      let a = 5
      let b = 3
      let c = 3
      let d = 2
      let e = 6
      let f = 4
      assert_equal(-31, -a + -b / -c * -d - -e * -f)
    }),
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

fn remainder_tests() -> List(Test) {
  [
    "1 % 1"
    |> example(fn() { assert_equal(0, 1 % 1) }),
    "1 % 0"
    |> example(fn() { assert_equal(0, 1 % 0) }),
    "3 % 2"
    |> example(fn() { assert_equal(1, 3 % 2) }),
    "3 % 0"
    |> example(fn() { assert_equal(0, 3 % 0) }),
    "3 % -2"
    |> example(fn() { assert_equal(1, 3 % -2) }),
    "3 % -0"
    |> example(fn() { assert_equal(0, 3 % -0) }),
    "-13 % 3"
    |> example(fn() { assert_equal(-1, -13 % 3) }),
    "13 % -3"
    |> example(fn() { assert_equal(1, 13 % -3) }),
    "-13 % -3"
    |> example(fn() { assert_equal(-1, -13 % -3) }),
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

fn bool_negation_tests() {
  [
    "!True"
    |> example(fn() { assert_equal(False, !True) }),
    "!False"
    |> example(fn() { assert_equal(True, !False) }),
    // This would crash if the right hand side evaluated
    "!True && panic"
    |> example(fn() { assert_equal(False, !True && panic) }),
    "!False || panic"
    |> example(fn() { assert_equal(True, !False || panic) }),
  ]
}

fn int_negation_tests() {
  [
    "-a"
    |> example(fn() {
      let a = 3
      let b = -a
      assert_equal(-3, b)
    }),
    "-{-a}"
    |> example(fn() {
      let a = 3
      let b = -{ -a }
      assert_equal(3, b)
    }),
    "-{-{-a}}"
    |> example(fn() {
      let a = 3
      let b = -{ -{ -a } }
      assert_equal(-3, b)
    }),
    "a - - b"
    |> example(fn() {
      let a = 3
      let b = -a
      let c = a - -b
      assert_equal(0, c)
    }),
    "a - - - - - - b"
    |> example(fn() {
      let a = 3
      let b = -a
      let c = a - -{ -{ -{ -{ -b } } } }
      assert_equal(0, c)
    }),
    "- a - {- {b}}"
    |> example(fn() {
      let a = 3
      let b = -a
      let c = -a - -b
      assert_equal(-6, c)
    }),
    "-abs(-6)"
    |> example(fn() {
      let abs = fn(value) {
        case value {
          value if value > 0 -> value
          _ -> -value
        }
      }
      assert_equal(-6, -abs(-6))
    }),
  ]
}

fn bit_array_match_tests() {
  [
    "let <<1, x>> = <<1, 2>>"
    |> example(fn() {
      assert_equal(2, {
        let assert <<1, x>> = <<1, 2>>
        x
      })
    }),
    "let <<a:8>> = <<1>>"
    |> example(fn() {
      assert_equal(1, {
        let assert <<a:8>> = <<1>>
        a
      })
    }),
    "let <<a:16, b:8>> = <<1, 2, 3>>"
    |> example(fn() {
      assert_equal(#(258, 3), {
        let assert <<a:16, b:8>> = <<1, 2, 3>>
        #(a, b)
      })
    }),
    "let <<a:float, b:int>> = <<63,240,0,0,0,0,0,0,1>>"
    |> example(fn() {
      assert_equal(#(1.0, 1), {
        let assert <<a:float, b:int>> = <<63, 240, 0, 0, 0, 0, 0, 0, 1>>
        #(a, b)
      })
    }),
    "let <<a:float>> = <<1.23:float>>"
    |> example(fn() {
      assert_equal(1.23, {
        let assert <<a:float>> = <<1.23:float>>
        a
      })
    }),
    "let <<_, rest:binary>> = <<1>>"
    |> example(fn() {
      assert_equal(<<>>, {
        let assert <<_, rest:bytes>> = <<1>>
        rest
      })
    }),
    "let <<_, rest:binary>> = <<1,2,3>>"
    |> example(fn() {
      assert_equal(<<2, 3>>, {
        let assert <<_, rest:bytes>> = <<1, 2, 3>>
        rest
      })
    }),
    "let <<x:2-binary, rest:binary>> = <<1,2,3>>"
    |> example(fn() {
      assert_equal(<<1, 2>>, {
        let assert <<x:2-bytes, _:bytes>> = <<1, 2, 3>>
        x
      })
    }),
    "bit_array from function"
    |> example(fn() {
      assert_equal(
        True,
        <<
          0x1,
          2,
          2:size(16),
          0x4:size(32),
          "Gleam":utf8,
          4.2:float,
          <<<<1, 2, 3>>:bits, "Gleam":utf8, 1024>>:bits,
        >> == importable.get_bit_array(),
      )
    }),
    "bit_array module const"
    |> example(fn() {
      assert_equal(
        True,
        <<
          0x1,
          2,
          2:size(16),
          0x4:size(32),
          "Gleam":utf8,
          4.2:float,
          <<<<1, 2, 3>>:bits, "Gleam":utf8, 1024>>:bits,
        >> == importable.data,
      )
    }),
    "<<71, 108, 101, 97, 109>> == <<\"Gleam\":utf8>>"
    |> example(fn() {
      assert_equal(True, <<71, 108, 101, 97, 109>> == <<"Gleam":utf8>>)
    }),
  ]
}

fn anonymous_function_tests() {
  [
    // https://github.com/gleam-lang/gleam/issues/1637
    "fn(x) { let x = x x }(1)"
    |> example(fn() {
      assert_equal(1, {
        let f = fn(x) {
          let x = x
          x
        }
        f(1)
      })
    }),
  ]
}

fn string_pattern_matching_tests() {
  [
    "case \"12345\" { \"0\" <> rest -> rest \"123\" <> rest -> rest _ -> \"\" }"
    |> example(fn() {
      assert_equal("45", case "12345" {
        "0" <> rest -> rest
        "123" <> rest -> rest
        _ -> ""
      })
    }),
    "match 🫥 test"
    |> example(fn() {
      assert_equal(" is neutral dotted", case "🫥 is neutral dotted" {
        "🫥" <> rest -> rest
        _ -> panic
      })
    }),
    "match Θ test"
    |> example(fn() {
      assert_equal(" foo bar", case "Θ foo bar" {
        "Θ" <> rest -> rest
        _ -> panic
      })
    }),
    "match 🇺🇸 test"
    |> example(fn() {
      assert_equal(" is a cluster", case "🇺🇸 is a cluster" {
        "🇺🇸" <> rest -> rest
        _ -> panic
      })
    }),
    "match backslash test"
    |> example(fn() {
      assert_equal(" is a backslash", case "\" is a backslash" {
        "\"" <> rest -> rest
        _ -> panic
      })
    }),
    "match newline test"
    |> example(fn() {
      assert_equal(" is a newline", case "\n is a newline" {
        "\n" <> rest -> rest
        _ -> panic
      })
    }),
    "match newline test"
    |> example(fn() {
      assert_equal(" is a newline that escaped", case
        "\\n is a newline that escaped"
      {
        "\\n" <> rest -> rest
        _ -> panic
      })
    }),
  ]
}

@target(javascript)
fn typescript_file_included_tests() {
  let path = "./build/dev/javascript/language/ffi_typescript.ts"
  [
    path
    |> example(fn() { assert_equal(file_exists(path), True) }),
  ]
}

@target(erlang)
fn typescript_file_included_tests() {
  let path = "./build/dev/erlang/language/_gleam_artefacts/ffi_typescript.ts"
  [
    path
    |> example(fn() { assert_equal(file_exists(path), True) }),
  ]
}

type Cat {
  Cat(String, cuteness: Int)
}

type NestedCat {
  NestedCat(Cat, String, cuteness: Int)
}

type InverseCat {
  InverseCat(cuteness: Int, String)
}

fn mixed_arg_match_tests() {
  [
    "matching second labelled arg as first"
    |> example(fn() {
      let Cat(cuteness: y, ..) = Cat("fluffy", 10)
      assert_equal(y, 10)
    }),
    "matching both args on position"
    |> example(fn() {
      let Cat(x, y) = Cat("fluffy", 10)
      assert_equal(#(x, y), #("fluffy", 10))
    }),
    "matching second labelled arg as second"
    |> example(fn() {
      let Cat(x, cuteness: y) = Cat("fluffy", 10)
      assert_equal(#(x, y), #("fluffy", 10))
    }),
    "nested custom types"
    |> example(fn() {
      let NestedCat(Cat(x, cuteness: y), cuteness: y2, ..) =
        NestedCat(Cat("fluffy", 10), "gleamy", 100)
      assert_equal(#(x, y, y2), #("fluffy", 10, 100))
    }),
    "matching first labelled arg as first"
    |> example(fn() {
      let InverseCat(cuteness: y, ..) = InverseCat(10, "fluffy")
      assert_equal(y, 10)
    }),
    "matching first labelled arg as second"
    |> example(fn() {
      let InverseCat(x, cuteness: y) = InverseCat(10, "fluffy")
      assert_equal(#(x, y), #("fluffy", 10))
    }),
  ]
}

fn block_tests() {
  [
    // https://github.com/gleam-lang/gleam/issues/1991
    "let x = 1 let _ = { let x = 2 x } x"
    |> example(fn() {
      assert_equal(
        {
          let x = 1
          let _ = {
            let x = 2
            x
          }
          x
        },
        1,
      )
    }),
  ]
}

type ContainsTuple {
  ContainsTuple(data: #(Int, #(Int, Person)))
}


fn tuple_access_tests() {
  [
    // https://github.com/gleam-lang/gleam/issues/1980
    "access regular tuple item"
    |> example(fn() {
      assert_equal(
        {
          let tup = #(3, 4, 5)
          let x = tup.0
          let y = tup.1
          let z = tup.2
          #(z, y, x)
        },
        #(5, 4, 3),
      )
    }),
    "access nested tuple item"
    |> example(fn() {
      assert_equal(
        {
          let tup = #(#(4, 5), #(6, 7))
          #(tup.0.1, tup.1.1, tup.1.0, tup.0.0)
        },
        #(5, 7, 6, 4),
      )
    }),
    "access deeply nested tuple item"
    |> example(fn() {
      assert_equal(
        {
          let tup = #(#(5, #(6, 7, #(8))))
          tup.0.1.2.0
        },
        8,
      )
    }),
    "access nested struct in a tuple item"
    |> example(fn() {
      assert_equal(
        {
          let tup = #(
            Person("Quinn", 27, "Canada"), 
            Person("Nikita", 99, "Internet"),
          )
          tup.0.name
        },
        "Quinn",
      )
    }),
    "access nested tuple in a struct"
    |> example(fn() {
      assert_equal(
        {
          let person = Person("Nikita", 99, "Internet")
          let container = ContainsTuple(#(5, #(6, person)))
          container.data.1.1.name
        },
        "Nikita",
      )
    }),
    "access tuple, then struct, then tuple"
    |> example(fn() {
      assert_equal(
        {
          let person = Person("Nikita", 99, "Internet")
          let container = ContainsTuple(#(5, #(6, person)))
          let tup = #(container)
          tup.0.data.0
        },
        5,
      )
    }),
  ]
}
