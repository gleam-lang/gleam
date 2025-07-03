//// Here are some things that have been previously been incorrectly reported as
//// unused.

import ffi.{file_exists}
import gleam
import importable.{NoFields}
import mod_with_numbers_0123456789
import record_update
import shadowed_module.{ShadowPerson}
import tests.{type Test, assert_equal, example, operator_test, suite}

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
      suite("bit arrays target", bit_array_target_tests()),
      suite("bit arrays", bit_array_tests()),
      suite("sized bit arrays", sized_bit_array_tests()),
      suite(
        "unaligned bit array expressions",
        unaligned_bit_array_expression_tests(),
      ),
      suite("unaligned bit array patterns", unaligned_bit_array_pattern_tests()),
      suite(
        "dynamic sized bit array patterns",
        dynamic_size_bit_array_pattern_tests(),
      ),
      suite("non UTF-8 string bit arrays", non_utf8_string_bit_array_tests()),
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
      suite("bit array match", bit_array_match_tests()),
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
    "1 + 1 == 2"
      |> example(fn() {
        assert_equal(0, case Nil {
          _ if 1 + 1 == 2 -> 0
          _ -> 1
        })
      }),
    "47 % 5 == 2"
      |> example(fn() {
        assert_equal(0, case Nil {
          _ if 47 % 5 == 2 -> 0
          _ -> 1
        })
      }),
    "3 * 5 == 15"
      |> example(fn() {
        assert_equal(0, case Nil {
          _ if 3 * 5 == 15 -> 0
          _ -> 1
        })
      }),
    "3 * 5 + 1 == 16"
      |> example(fn() {
        assert_equal(0, case Nil {
          _ if 3 * 5 + 1 == 16 -> 0
          _ -> 1
        })
      }),
    "1 + 3 * 5 == 16"
      |> example(fn() {
        assert_equal(0, case Nil {
          _ if 1 + 3 * 5 == 16 -> 0
          _ -> 1
        })
      }),
    "1 - 15 / 5 == -2"
      |> example(fn() {
        assert_equal(0, case Nil {
          _ if 1 - 15 / 5 == -2 -> 0
          _ -> 1
        })
      }),
    "15 / 5 - 1 == 2"
      |> example(fn() {
        assert_equal(0, case Nil {
          _ if 15 / 5 - 1 == 2 -> 0
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
    "<<\"😀\":utf8>> == <<\"\u{1F600}\":utf8>>"
      |> example(fn() {
        assert_equal(True, <<"😀":utf8>> == <<"\u{1F600}":utf8>>)
      }),
    "<<<<1>>:bit_array, 2>> == <<1, 2>>"
      |> example(fn() { assert_equal(True, <<<<1>>:bits, 2>> == <<1, 2>>) }),
    "<<1>> == <<1:int>>"
      |> example(fn() { assert_equal(True, <<1>> == <<1:int>>) }),
    "<<80_000:16>> == <<56, 128>>"
      |> example(fn() { assert_equal(True, <<80_000:16>> == <<56, 128>>) }),
    "<<-80_000:16>> == <<199, 128>>"
      |> example(fn() { assert_equal(True, <<-80_000:16>> == <<199, 128>>) }),
    "<<-1:64>> == <<255, 255, 255, 255, 255, 255, 255, 255>>"
      |> example(fn() {
        assert_equal(
          True,
          <<-1:64>> == <<255, 255, 255, 255, 255, 255, 255, 255>>,
        )
      }),
    "<<-489_391_639_457_909_760:56>> == <<53, 84, 229, 150, 16, 180, 0>>"
      |> example(fn() {
        assert_equal(
          True,
          <<-489_391_639_457_909_760:56>> == <<53, 84, 229, 150, 16, 180, 0>>,
        )
      }),
    "<<0, 0>> == <<0.0:float-16>>"
      |> example(fn() { assert_equal(True, <<0, 0>> == <<0.0:float-16>>) }),
    "<<0x00, 0xbc> == <<-1.0:float-16-little>>"
      |> example(fn() {
        assert_equal(True, <<0x00, 0xbc>> == <<-1.0:float-16-little>>)
      }),
    "<<0xf0, 0x3c> == <<1.234375:float-16-little>>"
      |> example(fn() {
        assert_equal(True, <<0xf0, 0x3c>> == <<1.234375:float-16-little>>)
      }),
    "<<0xfb, 0xff> == <<-65_504.0:float-16>>"
      |> example(fn() {
        assert_equal(True, <<0xfb, 0xff>> == <<-65_504.0:float-16>>)
      }),
    "<<63, 240, 0, 0, 0, 0, 0, 0>> == <<1.0:float>>"
      |> example(fn() {
        assert_equal(True, <<63, 240, 0, 0, 0, 0, 0, 0>> == <<1.0:float>>)
      }),
    "<<63, 128, 0, 0>> == <<1.0:float-32>>"
      |> example(fn() {
        assert_equal(True, <<63, 128, 0, 0>> == <<1.0:float-32>>)
      }),
    "<<0, 0, 0, 0, 0, 0, 240, 63>> == <<1.0:float-64-little>>"
      |> example(fn() {
        assert_equal(
          True,
          <<0, 0, 0, 0, 0, 0, 240, 63>> == <<1.0:float-64-little>>,
        )
      }),
    "<<63, 240, 0, 0, 0, 0, 0, 0>> == <<1.0:float-64-big>>"
      |> example(fn() {
        assert_equal(
          True,
          <<63, 240, 0, 0, 0, 0, 0, 0>> == <<1.0:float-64-big>>,
        )
      }),
    "pattern match on bit array containing utf8"
      |> example(fn() {
        assert_equal(True, case <<0x20, "😀👍":utf8, 0x20>> {
          <<" ":utf8, "😀👍":utf8, 0x20>> -> True
          _ -> False
        })
      }),
    "pattern match using `:bytes` on a sliced bit array"
      |> example(fn() {
        assert_equal(<<3, 4>>, {
          let assert <<_, b:bytes-3, _>> = <<1, 2, 3, 4, 5>>
          let assert <<_, rest:bytes>> = b
          rest
        })
      }),
    "matching zero length segment"
      |> example(fn() {
        let size = 0
        let data = <<>>
        assert_equal("ok", {
          case data {
            <<_:bytes-size(size), _:bytes>> -> "ok"
            _ -> "this cause should not be reached"
          }
        })
      }),
  ]
}

@target(erlang)
fn bit_array_target_tests() -> List(Test) {
  [
    "<<60, 0>> == <<1.0:float-16>>"
      |> example(fn() { assert_equal(True, <<60, 0>> == <<1.0:float-16>>) }),
    // https://github.com/gleam-lang/gleam/issues/3375
    "assignment int pattern in a bit array"
      |> example(fn() {
        assert_equal(10, {
          let assert <<10 as a, _>> = <<10, 20>>
          a
        })
      }),
    "assignment float pattern in a bit array"
      |> example(fn() {
        assert_equal(3.14, {
          let assert <<3.14 as pi:float>> = <<3.14>>
          pi
        })
      }),
    "assignment string pattern in a bit array"
      |> example(fn() {
        assert_equal("Hello", {
          let assert <<"Hello" as h:utf8, ", world!">> = <<"Hello, world!">>
          h
        })
      }),
    "pattern-match UTF-16 codepoint little-endian"
      |> example(fn() {
        assert_equal(ffi.utf_codepoint(127_757), {
          let assert <<codepoint:utf16_codepoint-little>> = <<"🌍":utf16-little>>
          codepoint
        })
      }),
    "pattern-match UTF-32 codepoint little-endian"
      |> example(fn() {
        assert_equal(ffi.utf_codepoint(127_757), {
          let assert <<codepoint:utf32_codepoint-little>> = <<"🌍":utf32-little>>
          codepoint
        })
      }),
  ]
}

@target(javascript)
fn bit_array_target_tests() -> List(Test) {
  []
}

fn sized_bit_array_tests() -> List(Test) {
  [
    "<<257:size(8)>> == <<1>>"
      |> example(fn() { assert_equal(True, <<257:size(8)>> == <<1>>) }),
    "let i = 257\n<<i:size(8)>> == <<1>>"
      |> example(fn() {
        let i = 257
        assert_equal(True, <<i:size(8)>> == <<1>>)
      }),
    "<<257:size(16)>> == <<1, 1>>"
      |> example(fn() { assert_equal(True, <<257:size(16)>> == <<1, 1>>) }),
    "let i = 257\n<<i:size(16)>> == <<1, 1>>"
      |> example(fn() {
        let i = 257
        assert_equal(True, <<i:size(16)>> == <<1, 1>>)
      }),
    "<<257:size(24)>> == <<1, 1>>"
      |> example(fn() { assert_equal(True, <<257:size(24)>> == <<0, 1, 1>>) }),
    "let i = 257\n<<i:size(24)>> == <<1, 1>>"
      |> example(fn() {
        let i = 257
        assert_equal(True, <<i:size(24)>> == <<0, 1, 1>>)
      }),
    "<<4294967297:size(40)>> == <<1, 0, 0, 0, 1>>"
      |> example(fn() {
        assert_equal(True, <<4_294_967_297:size(40)>> == <<1, 0, 0, 0, 1>>)
      }),
    "let i = 4294967297\n<<i:size(40)>> == <<1, 0, 0, 0, 1>>"
      |> example(fn() {
        let i = 4_294_967_297
        assert_equal(True, <<i:size(40)>> == <<1, 0, 0, 0, 1>>)
      }),
    "<<100_000:24-little>> == <<160, 134, 1>>"
      |> example(fn() {
        assert_equal(True, <<100_000:24-little>> == <<160, 134, 1>>)
      }),
    "let i = 100_000\n<<i:24-little>> == <<160, 134, 1>>"
      |> example(fn() {
        let i = 100_000
        assert_equal(True, <<i:24-little>> == <<160, 134, 1>>)
      }),
    "<<-1:32-big>> == <<255, 255, 255, 255>>"
      |> example(fn() {
        assert_equal(True, <<-1:32-big>> == <<255, 255, 255, 255>>)
      }),
    "let i = -1\n<<i:32-big>> == <<255, 255, 255, 255>>"
      |> example(fn() {
        let i = -1
        assert_equal(True, <<i:32-big>> == <<255, 255, 255, 255>>)
      }),
    "<<100_000_000_000:32-little>> == <<0, 232, 118, 72>>"
      |> example(fn() {
        assert_equal(True, <<100_000_000_000:32-little>> == <<0, 232, 118, 72>>)
      }),
    "let i = 100_000_000_000\n<<i:32-little>> == <<0, 232, 118, 72>>"
      |> example(fn() {
        let i = 100_000_000_000
        assert_equal(True, <<i:32-little>> == <<0, 232, 118, 72>>)
      }),
    "<<256:size(-1)>> == <<>>"
      |> example(fn() { assert_equal(True, <<>> == <<256:size(-1)>>) }),
    "let i = 256\n<<i:size(-1)>> == <<>>"
      |> example(fn() {
        let i = 256
        assert_equal(True, <<i:size(-1)>> == <<>>)
      }),
    // JS Number.MAX_SAFE_INTEGER
    "<<9_007_199_254_740_991:size(64)>> == <<0, 31, 255, 255, 255, 255, 255, 255>>"
      |> example(fn() {
        assert_equal(
          True,
          <<9_007_199_254_740_991:size(64)>>
            == <<0, 31, 255, 255, 255, 255, 255, 255>>,
        )
      }),
    "let i = 9_007_199_254_740_991\n<<i:size(64)>> == <<0, 31, 255, 255, 255, 255, 255, 255>>"
      |> example(fn() {
        let i = 9_007_199_254_740_991
        assert_equal(
          True,
          <<i:size(64)>> == <<0, 31, 255, 255, 255, 255, 255, 255>>,
        )
      }),
    "let i = 9_007_199_254_740_991\n<<i:size(4)-unit(16)>> == <<0, 31, 255, 255, 255, 255, 255, 255>>"
      |> example(fn() {
        let i = 9_007_199_254_740_991
        assert_equal(
          True,
          <<i:size(4)-unit(16)>> == <<0, 31, 255, 255, 255, 255, 255, 255>>,
        )
      }),
    "let size = 5\n<<405:size(size)-unit(2)>> == <<101, 1:2>>"
      |> example(fn() {
        let size = 5
        assert_equal(True, <<405:size(size)-unit(2)>> == <<101, 1:2>>)
      }),
    "let assert <<len, payload:bits-size(len * 8 - 4)>>"
      |> example(fn() {
        assert_equal(<<1, 2, 3, 4:4>>, {
          let assert <<len, payload:bits-size(len * 8 - 4)>> = <<
            4, 1, 2, 3, 4:4,
          >>
          payload
        })
      }),
    "let assert <<len, payload:bytes-size(len / 8 + 2)>>"
      |> example(fn() {
        assert_equal(<<1, 2, 3, 4, 5, 6>>, {
          let assert <<len, payload:bytes-size(len / 8 + 2)>> = <<
            32, 1, 2, 3, 4, 5, 6,
          >>
          payload
        })
      }),
    "let additional = 5\nlet assert <<len, payload:bits-size(len + additional * 8)>>"
      |> example(fn() {
        assert_equal(<<1, 2, 3, 4, 5, 6>>, {
          let additional = 5
          let assert <<len, payload:bits-size(len + additional * 8)>> = <<
            8, 1, 2, 3, 4, 5, 6,
          >>
          payload
        })
      }),
    "let assert <<len, payload:bits-size({ len + 1 } * 8)>>"
      |> example(fn() {
        assert_equal(<<1, 2, 3, 4>>, {
          let assert <<len, payload:bits-size({ len + 1 } * 8)>> = <<
            3, 1, 2, 3, 4,
          >>
          payload
        })
      }),
    "Pattern match on negative size"
      |> example(fn() {
        assert_equal(2, {
          case <<1, 2, 3, 4>> {
            <<a, b:size(a - 100_000), _c:size(b)>> -> 1
            _ -> 2
          }
        })
      }),
  ]
}

fn unaligned_bit_array_expression_tests() -> List(Test) {
  [
    "<<0xFF:6, 0:2>> == <<0xFA>>"
      |> example(fn() { assert_equal(True, <<0xFF:6, 0:2>> == <<0xFC>>) }),
    "<<0xFF:6, 0:3, 0x75:9>> == <<252, 29, 1:2>>"
      |> example(fn() {
        assert_equal(True, <<0xFF:6, 0:3, 0x75:9>> == <<252, 29, 1:2>>)
      }),
    "<<-1:55, 44:11-little, 0x75:9-big>> == <<255, 255, 255, 255, 255, 255, 254, 88, 14, 5:3>>"
      |> example(fn() {
        assert_equal(
          True,
          <<-1:55, 44:11-little, 0x75:9-big>>
            == <<255, 255, 255, 255, 255, 255, 254, 88, 14, 5:3>>,
        )
      }),
    "<<0:1, 2:2, 2:3, 1:1>> == <<0b0100101:7>>"
      |> example(fn() {
        assert_equal(True, <<0:1, 2:2, 2:3, 1:1>> == <<0b0100101:7>>)
      }),
    "<<-100:6, -10:32-little, -10:32-big, -100:48-big, -100:48-little>> == <<115, 219, 255, 255, 255, 255, 255, 255, 219, 255, 255, 255, 255, 254, 114, 115, 255, 255, 255, 255, 63:6>>"
      |> example(fn() {
        assert_equal(
          True,
          <<-100:6, -10:32-little, -10:32-big, -100:48-big, -100:48-little>>
            == <<
            115, 219, 255, 255, 255, 255, 255, 255, 219, 255, 255, 255, 255, 254,
            114, 115, 255, 255, 255, 255, 63:6,
          >>,
        )
      }),
    "<<2:3, 2.9283123:float-little, -1.375e5:32-float-big>> == <<91, 153, 120, 255, 229, 205, 160, 232, 25, 0, 200, 224, 0:3>>"
      |> example(fn() {
        assert_equal(
          True,
          <<2:3, 2.9283123:float-little, -1.375e5:32-float-big>>
            == <<91, 153, 120, 255, 229, 205, 160, 232, 25, 0, 200, 224, 0:3>>,
        )
      }),
    "<<7:6, <<1:3>>:bits, <<1, 2, 3>>:bits, 1:1, <<-1124.789e4:float-little>>:bits>> == <<28, 128, 129, 1, 192, 0, 0, 16, 8, 157, 25, 112, 1:2>>"
      |> example(fn() {
        assert_equal(
          True,
          <<
            7:6,
            <<1:3>>:bits,
            <<1, 2, 3>>:bits,
            1:1,
            <<-1124.789e4:float-little>>:bits,
          >>
            == <<28, 128, 129, 1, 192, 0, 0, 16, 8, 157, 25, 112, 1:2>>,
        )
      }),
    "<<9_444_732_965_739_289_353_650_176:75>> == <<255, 255, 255, 255, 255, 248, 0, 0, 0, 0:size(3)>>"
      |> example(fn() {
        assert_equal(
          True,
          <<9_444_732_965_739_289_353_650_176:75>>
            == <<255, 255, 255, 255, 255, 248, 0, 0, 0, 0:size(3)>>,
        )
      }),
    "<<0xFC:6>> == <<<<0xFC>>:bits-6>>"
      |> example(fn() {
        assert_equal(True, <<0b100101:6>> == <<<<0b10010100>>:bits-6>>)
      }),
    "<<0xE7>> == <<<<0xEC>>:bits-4, 7:4>>"
      |> example(fn() {
        assert_equal(True, <<0xE7>> == <<<<0xEC>>:bits-4, 7:4>>)
      }),
    "<<0b11001:5>> == <<<<0b11011100>>:bits-size(three), 1:size(two)>>"
      |> example(fn() {
        let three = 3
        let two = 2
        assert_equal(
          True,
          <<0b11001:5>> == <<<<0b11011100>>:bits-size(three), 1:size(two)>>,
        )
      }),
  ]
}

fn unaligned_bit_array_pattern_tests() -> List(Test) {
  [
    "let assert <<a:4, b:4, c:3, d:4, e:1>> = <<0xAB, 0b11010101>>"
      |> example(fn() {
        assert_equal(#(0xA, 0xB, 0b110, 0b1010, 0b1), {
          let assert <<a:4, b:4, c:3, d:4, e:1>> = <<0xAB, 0b11010101>>
          #(a, b, c, d, e)
        })
      }),
    "let assert <<a:12, b:4>> = <<0xB6, 0xE3>>"
      |> example(fn() {
        assert_equal(#(0xB6E, 0x03), {
          let assert <<a:12, b:4>> = <<0xB6, 0xE3>>
          #(a, b)
        })
      }),
    "let assert <<a:4, b:12>> = <<0xB6, 0xE3>>"
      |> example(fn() {
        assert_equal(#(0x0B, 0x6E3), {
          let assert <<a:4, b:12>> = <<0xB6, 0xE3>>
          #(a, b)
        })
      }),
    "let assert <<a:12-little, b:4>> = <<0xB6, 0xE3>>"
      |> example(fn() {
        assert_equal(#(0xEB6, 0x03), {
          let assert <<a:12-little, b:4>> = <<0xB6, 0xE3>>
          #(a, b)
        })
      }),
    "let assert <<a:4, b:12-little>> = <<0xB6, 0xE3>>"
      |> example(fn() {
        assert_equal(#(0x0B, 0x36E), {
          let assert <<a:4, b:12-little>> = <<0xB6, 0xE3>>
          #(a, b)
        })
      }),
    "let assert <<a:5, b:11>> = <<0xB6, 0xE3>>"
      |> example(fn() {
        assert_equal(#(22, 1763), {
          let assert <<a:5, b:11>> = <<0xB6, 0xE3>>
          #(a, b)
        })
      }),
    "let assert <<_:8, a:17>> = <<0xFF, 0xB6, 0xE3, 1:1>>"
      |> example(fn() {
        assert_equal(93_639, {
          let assert <<_:8, a:17>> = <<0xFF, 0xB6, 0xE3, 1:1>>
          a
        })
      }),
    "let assert <<a:17, _:bits>> = <<0xA6, 0xE3, 6:3>>"
      |> example(fn() {
        assert_equal(85_447, {
          let assert <<a:17, _:bits>> = <<0xA6, 0xE3, 6:3>>
          a
        })
      }),
    "let assert <<a:17, _:bits>> = <<0xA6, 0xE3, 6:3>>"
      |> example(fn() {
        assert_equal(85_447, {
          let assert <<a:17, _:bits>> = <<0xA6, 0xE3, 6:3>>
          a
        })
      }),
    "let assert <<a:7-signed, _:bits>> = <<0b10011010>>"
      |> example(fn() {
        assert_equal(-51, {
          let assert <<a:7-signed, _:bits>> = <<0b10011010>>
          a
        })
      }),
    "let assert <<_:5, a:13-big, _:bits>> = <<0xA5, 0x6C, 0xAA>>"
      |> example(fn() {
        assert_equal(5554, {
          let assert <<_:5, a:13-big, _:bits>> = <<0xA5, 0x6C, 0xAA>>
          a
        })
      }),
    "let assert <<_:5, a:13-little-signed, _:bits>> = <<0xA5, 0x6C, 0xAA>>"
      |> example(fn() {
        assert_equal(-3411, {
          let assert <<_:5, a:13-little-signed, _:bits>> = <<0xA5, 0x6C, 0xAA>>
          a
        })
      }),
    "let assert <<_:3, a:8-big-signed, _:bits>> = <<253, 94>>"
      |> example(fn() {
        assert_equal(-22, {
          let assert <<_:3, a:8-big-signed, _:bits>> = <<253, 94>>
          a
        })
      }),
    "let assert <<_:3, a:12-big-unsigned, _:bits>> = <<233, 164>>"
      |> example(fn() {
        assert_equal(1234, {
          let assert <<_:3, a:12-big-unsigned, _:bits>> = <<233, 164>>
          a
        })
      }),
    "let assert <<_:3, a:12-little, _:bits>> = <<250, 72>>"
      |> example(fn() {
        assert_equal(1234, {
          let assert <<_:3, a:12-little, _:bits>> = <<250, 72>>
          a
        })
      }),
    "let assert <<_:7, a:22, _:bits>> = <<250, 72, 223, 189>>"
      |> example(fn() {
        assert_equal(596_983, {
          let assert <<_:7, a:22, _:bits>> = <<250, 72, 223, 189>>
          a
        })
      }),
    "let assert <<_:1, a:23, _:bits>> = <<146, 192, 70, 25, 1:1>>"
      |> example(fn() {
        assert_equal(1_228_870, {
          let assert <<_:1, a:23, _:bits>> = <<146, 192, 70, 25, 1:1>>
          a
        })
      }),
    "let assert <<_:1, a:32>> = <<217, 150, 209, 191, 0:1>>"
      |> example(fn() {
        assert_equal(3_006_112_638, {
          let assert <<_:1, a:32>> = <<217, 150, 209, 191, 0:1>>
          a
        })
      }),
    "let assert <<_:1, a:32-signed>> = <<146, 192, 70, 25, 1:1>>"
      |> example(fn() {
        assert_equal(629_181_491, {
          let assert <<_:1, a:32-signed>> = <<146, 192, 70, 25, 1:1>>
          a
        })
      }),
    "let assert <<_:1, a:32-little-unsigned>> = <<251, 24, 47, 227, 1:1>>"
      |> example(fn() {
        assert_equal(3_344_904_438, {
          let assert <<_:1, a:32-little-unsigned>> = <<251, 24, 47, 227, 1:1>>
          a
        })
      }),
    "let assert <<a:33-little-unsigned>> = <<240, 102, 91, 101, 1:1>>"
      |> example(fn() {
        assert_equal(5_995_456_240, {
          let assert <<a:33-little-unsigned>> = <<240, 102, 91, 101, 1:1>>
          a
        })
      }),
    "let assert <<a:40-big-signed, _:8>> = <<231, 255, 255, 255, 254, 123>>"
      |> example(fn() {
        assert_equal(-103_079_215_106, {
          let assert <<a:40-big-signed, _:8>> = <<231, 255, 255, 255, 254, 123>>
          a
        })
      }),
    "let assert <<_:1, a:54-big-unsigned, _:bits>> = <<0, 231, 255, 255, 253, 123, 17>>"
      |> example(fn() {
        assert_equal(127_543_348_739_464, {
          let assert <<_:1, a:54-big-unsigned, _:bits>> = <<
            0, 231, 255, 255, 253, 123, 17,
          >>
          a
        })
      }),
    "let assert <<_:8, a:54-little-signed, _:bits>> = <<142, 231, 255, 255, 253, 123, 17, 139>>"
      |> example(fn() {
        assert_equal(-8_425_025_061_257_241, {
          let assert <<_:8, a:54-little-signed, _:bits>> = <<
            142, 231, 255, 255, 253, 123, 17, 139,
          >>
          a
        })
      }),
    "let assert <<_:7, a:55-little-signed, _:bits>> = <<142, 231, 255, 255, 253, 123, 17, 139>>"
      |> example(fn() {
        assert_equal(-8_293_899_692_933_261, {
          let assert <<_:7, a:55-little-signed, _:bits>> = <<
            142, 231, 255, 255, 253, 123, 17, 139,
          >>
          a
        })
      }),
    "let assert <<_:8, a:40-big-signed, _:8>> = <<142, 231, 255, 255, 253, 123, 17>>"
      |> example(fn() {
        assert_equal(-103_079_215_749, {
          let assert <<_:8, a:40-big-signed, _:8>> = <<
            142, 231, 255, 255, 253, 123, 17,
          >>
          a
        })
      }),
    "let assert <<_:14, a:71-little-signed, _:bits>> = <<250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177>>"
      |> example(fn() {
        assert_equal(70_821_197_049_655, {
          let assert <<_:14, a:71-little-signed, _:bits>> = <<
            250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177,
          >>
          a
        })
      }),
    "let assert <<_:14, a:71-big-signed, _:bits>> = <<250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177>>"
      |> example(fn() {
        assert_equal(515_906_807_693_217_628_160, {
          let assert <<_:14, a:71-big-signed, _:bits>> = <<
            250, 72, 223, 189, 41, 97, 165, 0, 0, 0, 0, 177,
          >>
          a
        })
      }),
    "let assert <<_:11, a:float-32, _:bits>> = <<112, 152, 127, 244, 0, 7, 0:2>>"
      |> example(fn() {
        assert_equal(-511.25, {
          let assert <<_:11, a:float-32, _:bits>> = <<
            112, 152, 127, 244, 0, 7, 0:2,
          >>
          a
        })
      }),
    "let assert <<_:7, a:float-little, _:bits>> = <<8, 0, 0, 0, 1, 129, 39, 103, 129, 127:7>>"
      |> example(fn() {
        assert_equal(-5011.75, {
          let assert <<_:7, a:float-little, _:bits>> = <<
            8, 0, 0, 0, 1, 129, 39, 103, 129, 127:7,
          >>
          a
        })
      }),
    "let assert <<a:bits-7, b:bits-3>> = <<0b11001011, 0b01:2>>"
      |> example(fn() {
        assert_equal(#(<<0b1100101:7>>, <<0b101:3>>), {
          let assert <<a:bits-7, b:bits-3>> = <<0b11001011, 0b01:2>>
          #(a, b)
        })
      }),
    "let assert <<a:bits-16, b:bits-13, c:bits-11, d:bits-24>> = <<0x47, 0x9A, 0x25, 0x0C, 0xDA, 0xF1, 0xEE, 0x31>>"
      |> example(fn() {
        assert_equal(
          #(<<71, 154>>, <<37, 1:5>>, <<155, 2:3>>, <<0xF1, 0xEE, 0x31>>),
          {
            let assert <<a:bits-16, b:bits-13, c:bits-11, d:bits-24>> = <<
              0x47, 0x9A, 0x25, 0x0C, 0xDA, 0xF1, 0xEE, 0x31,
            >>
            #(a, b, c, d)
          },
        )
      }),
    "let assert <<a:bits-3, b:bytes-2, _:bytes>> = <<0b110:3, 0x12, 0xAB, 0x95, 0xFE>>"
      |> example(fn() {
        assert_equal(#(<<0b110:3>>, <<0x12, 0xAB>>, <<0x95, 0xFE>>), {
          let assert <<a:bits-3, b:bytes-2, c:bytes>> = <<
            0b110:3, 0x12, 0xAB, 0x95, 0xFE,
          >>
          #(a, b, c)
        })
      }),
    "let assert <<0x12, _:5, _:bytes>> = <<0x12, 0xFF:6, 0x95, 0xFE>>"
      |> example(fn() {
        assert_equal(False, {
          case <<0x12, 0xAB, 0x95, 0xFE>> {
            <<0x34, _:5, _:bytes>> -> True
            _ -> False
          }
        })
      }),
  ]
}

fn dynamic_size_bit_array_pattern_tests() -> List(Test) {
  [
    "let size = 8
let assert <<value:size(size)>> = <<42>>"
      |> example(fn() {
        assert_equal(42, {
          let size = 8
          let assert <<value:size(size)>> = <<42>>
          value
        })
      }),
    "let size = 3
let other_size = 5
let third_size = 8
let assert <<value:size(size), second:size(other_size), last:size(third_size)>> = <<5:3, 8:5, 128>>"
      |> example(fn() {
        assert_equal(#(5, 8, 128), {
          let size = 3
          let other_size = 5
          let third_size = 8
          let assert <<
            value:size(size),
            second:size(other_size),
            last:size(third_size),
          >> = <<5:3, 8:5, 128>>
          #(value, second, last)
        })
      }),
    "let size = 2
let assert <<first_bytes:bytes-size(size), rest:bytes>> = <<1, 2, 3, 4>>"
      |> example(fn() {
        assert_equal(<<1, 2>>, {
          let size = 2
          let assert <<first_bytes:bytes-size(size), _rest:bytes>> = <<
            1, 2, 3, 4,
          >>
          first_bytes
        })
      }),
    "let size = 6
let assert <<bits:bits-size(size), rest:bits>> = <<0b10010100, 6, 2938>>"
      |> example(fn() {
        assert_equal(<<0b100101:6>>, {
          let size = 6
          let assert <<bits:bits-size(size), _rest:bits>> = <<
            0b10010100, 6, 2938,
          >>
          bits
        })
      }),
    "let size = 7
let assert <<123:size(size)>> = <<123:7>>"
      |> example(fn() {
        let size = 7
        let assert <<123:size(size)>> = <<123:7>>
        tests.pass()
      }),
    "let size = 4
let size = size + 2
let assert <<value:size(size)>> = <<61:6>>"
      |> example(fn() {
        assert_equal(61, {
          let size = 4
          let size = size + 2
          let assert <<value:size(size)>> = <<61:6>>
          value
        })
      }),
  ]
}

fn non_utf8_string_bit_array_tests() -> List(Test) {
  [
    "let assert <<\"Hello, world\":utf16>> = <<\"Hello, world\":utf16>>"
      |> example(fn() {
        assert_equal(<<"Hello, world":utf16>>, {
          let assert <<"Hello, world":utf16>> = <<"Hello, world":utf16>>
        })
      }),
    "let assert <<\"Hello, world\":utf32>> = <<\"Hello, world\":utf32>>"
      |> example(fn() {
        assert_equal(<<"Hello, world":utf32>>, {
          let assert <<"Hello, world":utf32>> = <<"Hello, world":utf32>>
        })
      }),
    "UTF-16 bytes"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf16>>, <<
          0, 72, 0, 101, 0, 108, 0, 108, 0, 111, 0, 44, 0, 32, 216, 60, 223, 13,
          0, 33,
        >>)
      }),
    "UTF-32 bytes"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf32>>, <<
          0, 0, 0, 72, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 111, 0,
          0, 0, 44, 0, 0, 0, 32, 0, 1, 243, 13, 0, 0, 0, 33,
        >>)
      }),
    "UTF-16 pattern matching"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf16>>, {
          let assert <<"Hello, 🌍!":utf16>> = <<
            0, 72, 0, 101, 0, 108, 0, 108, 0, 111, 0, 44, 0, 32, 216, 60, 223,
            13, 0, 33,
          >>
        })
      }),
    "UTF-32 pattern matching"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf32>>, {
          let assert <<"Hello, 🌍!":utf32>> = <<
            0, 0, 0, 72, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 111,
            0, 0, 0, 44, 0, 0, 0, 32, 0, 1, 243, 13, 0, 0, 0, 33,
          >>
        })
      }),
    "UTF-16 bytes little endian"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf16-little>>, <<
          72, 0, 101, 0, 108, 0, 108, 0, 111, 0, 44, 0, 32, 0, 60, 216, 13, 223,
          33, 0,
        >>)
      }),
    "UTF-32 bytes little endian"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf32-little>>, <<
          72, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 111, 0, 0, 0,
          44, 0, 0, 0, 32, 0, 0, 0, 13, 243, 1, 0, 33, 0, 0, 0,
        >>)
      }),
    "UTF-16 pattern matching little endian"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf16-little>>, {
          let assert <<"Hello, 🌍!":utf16-little>> = <<
            72, 0, 101, 0, 108, 0, 108, 0, 111, 0, 44, 0, 32, 0, 60, 216, 13,
            223, 33, 0,
          >>
        })
      }),
    "UTF-32 pattern matching little endian"
      |> example(fn() {
        assert_equal(<<"Hello, 🌍!":utf32-little>>, {
          let assert <<"Hello, 🌍!":utf32-little>> = <<
            72, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 111, 0, 0, 0,
            44, 0, 0, 0, 32, 0, 0, 0, 13, 243, 1, 0, 33, 0, 0, 0,
          >>
        })
      }),
    "UTF-16 codepoint"
      |> example(fn() {
        assert_equal(<<216, 60, 223, 13>>, {
          // 🌍
          let codepoint = ffi.utf_codepoint(127_757)
          <<codepoint:utf16_codepoint>>
        })
      }),
    "UTF-16 codepoint little-endian"
      |> example(fn() {
        assert_equal(<<60, 216, 13, 223>>, {
          // 🌍
          let codepoint = ffi.utf_codepoint(127_757)
          <<codepoint:utf16_codepoint-little>>
        })
      }),
    "UTF-32 codepoint"
      |> example(fn() {
        assert_equal(<<0, 1, 243, 13>>, {
          // 🌍
          let codepoint = ffi.utf_codepoint(127_757)
          <<codepoint:utf32_codepoint>>
        })
      }),
    "UTF-32 codepoint little-endian"
      |> example(fn() {
        assert_equal(<<13, 243, 1, 0>>, {
          // 🌍
          let codepoint = ffi.utf_codepoint(127_757)
          <<codepoint:utf32_codepoint-little>>
        })
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
    "let <<a:int-32-little-signed, b:signed-big-24>> = <<255, 255, 255, 255, 240, 216, 255>>"
      |> example(fn() {
        assert_equal(#(-1, -10_000), {
          let assert <<a:int-32-little-signed, b:signed-big-24>> = <<
            255, 255, 255, 255, 255, 216, 240,
          >>
          #(a, b)
        })
      }),
    "let <<a:16-unsigned, b:40-signed-little>> = <<255, 255, 255, 255, 240, 216, 255>>"
      |> example(fn() {
        assert_equal(#(65_535, -655_294_465), {
          let assert <<a:16-unsigned, b:40-signed-little>> = <<
            255, 255, 255, 255, 240, 216, 255,
          >>
          #(a, b)
        })
      }),
    "let <<a:64-signed>> = <<255, 255, 255, 255, 255, 255, 255, 255>>"
      |> example(fn() {
        assert_equal(-1, {
          let assert <<a:64-signed>> = <<
            255, 255, 255, 255, 255, 255, 255, 255,
          >>
          a
        })
      }),
    "let <<a:56-big-unsigned>> = <<0x00, 0xaa, 255, 255, 255, 255, 255>>"
      |> example(fn() {
        assert_equal(0xaaffffffffff, {
          let assert <<a:56-big-unsigned>> = <<
            0x00, 0xaa, 255, 255, 255, 255, 255,
          >>
          a
        })
      }),
    "let <<a:56-little-unsigned>> = <<255, 255, 255, 255, 255, 0xaa, 0x00>>"
      |> example(fn() {
        assert_equal(0xaaffffffffff, {
          let assert <<a:56-little-unsigned>> = <<
            255, 255, 255, 255, 255, 0xaa, 0x00,
          >>
          a
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
    "let <<a:float-32>> = <<63, 176, 0, 0>>"
      |> example(fn() {
        assert_equal(1.375, {
          let assert <<a:float-32>> = <<63, 176, 0, 0>>
          a
        })
      }),
    "let <<a:64-float-little>> = <<61, 10, 215, 163, 112, 61, 18, 64>>"
      |> example(fn() {
        assert_equal(4.56, {
          let assert <<a:64-float-little>> = <<
            61, 10, 215, 163, 112, 61, 18, 64,
          >>
          a
        })
      }),
    "let <<a:float-16>> = <<0x0C, 0x00>>"
      |> example(fn() {
        assert_equal(0.0, {
          let assert <<a:float-16>> = <<0, 0>>
          a
        })
      }),
    "let <<a:float-16>> = <<0x3C, 0xF0>>"
      |> example(fn() {
        assert_equal(1.234375, {
          let assert <<a:float-16>> = <<0x3C, 0xF0>>
          a
        })
      }),
    "let <<a:float-16-little>> = <<0xFF, 0xFB>>"
      |> example(fn() {
        assert_equal(-65_504.0, {
          let assert <<a:float-16-little>> = <<0xFF, 0xFB>>
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
          >>
            == importable.get_bit_array(),
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
          >>
            == importable.data,
        )
      }),
    "<<71, 108, 101, 97, 109>> == <<\"Gleam\":utf8>>"
      |> example(fn() {
        assert_equal(True, <<71, 108, 101, 97, 109>> == <<"Gleam":utf8>>)
      }),
    "<<i:40-signed, _:bits>> == <<231, 255, 255, 255, 254, 123>>"
      |> example(fn() {
        assert_equal(-103_079_215_106, {
          let assert <<i:40-signed, _:bits>> = <<231, 255, 255, 255, 254, 123>>
          i
        })
      }),
    "<<_, i:40-signed, _:bits>> == <<142, 231, 255, 255, 253, 123, 17>>"
      |> example(fn() {
        assert_equal(-103_079_215_749, {
          let assert <<_, i:40-signed, _:bits>> = <<
            142, 231, 255, 255, 253, 123, 17,
          >>
          i
        })
      }),
    // https://github.com/gleam-lang/gleam/issues/4712
    "Multiple variable segments"
      |> example(fn() {
        assert_equal(12, {
          let assert <<a, b:size(a), c:size(b)>> = <<2, 3:2, 7:3>>
          a + b + c
        })
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
        assert_equal(" wibble wobble", case "Θ wibble wobble" {
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
        assert_equal(
          " is a newline that escaped",
          case "\\n is a newline that escaped" {
            "\\n" <> rest -> rest
            _ -> panic
          },
        )
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
            { tup.0 }.name
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
            { container.data.1.1 }.name
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
            { tup.0 }.data.0
          },
          5,
        )
      }),
  ]
}
