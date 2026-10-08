// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

fn count_down(from i) {
  case i {
    0 -> Nil
    _ -> count_down(i - 1)
  }
}

fn tail_recursive_accumulate_down(x, y) {
  case x {
    0 -> y
    _ -> tail_recursive_accumulate_down(x - 1, [x, ..y])
  }
}

fn function_shadowed_by_own_argument(function_shadowed_by_own_argument) {
  function_shadowed_by_own_argument()
}

fn recursive_argument_closure(x, read) {
  case x {
    0 -> read()
    _ -> recursive_argument_closure(x - 1, fn() { x })
  }
}

fn rotate_arguments(x, y, remaining) {
  case remaining {
    0 -> #(x, y)
    _ -> rotate_arguments(y, x, remaining - 1)
  }
}

fn nested_recursive_arguments(x, y, remaining) {
  case remaining {
    0 -> x + y
    _ ->
      nested_recursive_arguments(
        x - 1,
        nested_recursive_arguments(x, y, 0),
        remaining - 1,
      )
  }
}

fn discarded_recursive_argument(x, _) {
  case x {
    0 -> Nil
    _ ->
      discarded_recursive_argument(x - 1, {
        assert x > 0
        Nil
      })
  }
}

fn recursive_guard_closure(x, read) {
  case x {
    0 -> read()
    _ ->
      recursive_guard_closure(x - 1, fn() {
        case Nil {
          _ if x == 1 -> True
          _ -> False
        }
      })
  }
}

fn recursive_guard_argument(x, result) {
  case x {
    0 -> result
    _ ->
      recursive_guard_argument(x - 1, case Nil {
        _ if x == 1 -> True
        _ -> False
      })
  }
}

fn recursive_size_closure(x, read) {
  case x {
    0 -> read()
    _ ->
      recursive_size_closure(x - 1, fn() {
        let assert <<value:size(x), _:bits>> = <<128>>
        value
      })
  }
}

fn recursive_size_argument(x, result) {
  case x {
    0 -> result
    _ ->
      recursive_size_argument(x - 1, {
        let assert <<value:size(x), _:bits>> = <<128>>
        value
      })
  }
}

pub fn ten_million_recursions_doesnt_overflow_the_stack_test() {
  assert Nil == count_down(from: 10_000_000)
}

// https://github.com/gleam-lang/gleam/issues/1214
// https://github.com/gleam-lang/gleam/issues/1380
pub fn arguments_correctly_reassigned_test() {
  assert [1, 2, 3] == tail_recursive_accumulate_down(3, [])
}

pub fn recursive_argument_closure_captures_fresh_binding_test() {
  assert 1 == recursive_argument_closure(2, fn() { 0 })
}

pub fn recursive_arguments_are_updated_simultaneously_test() {
  assert #(2, 1) == rotate_arguments(1, 2, 1)
  assert #(1, 2) == rotate_arguments(1, 2, 2)
}

pub fn nested_recursive_arguments_read_previous_values_test() {
  assert 6 == nested_recursive_arguments(2, 3, 1)
  assert 10 == nested_recursive_arguments(3, 4, 2)
}

pub fn discarded_recursive_argument_reads_previous_value_test() {
  assert Nil == discarded_recursive_argument(2, Nil)
}

pub fn recursive_guard_closure_captures_fresh_binding_test() {
  assert True == recursive_guard_closure(2, fn() { False })
}

pub fn recursive_guard_argument_reads_previous_value_test() {
  assert True == recursive_guard_argument(2, False)
}

pub fn recursive_size_closure_captures_fresh_binding_test() {
  assert 1 == recursive_size_closure(2, fn() { 0 })
}

pub fn recursive_size_argument_reads_previous_value_test() {
  assert 1 == recursive_size_argument(2, 0)
}

// https://github.com/gleam-lang/gleam/issues/2400
pub fn function_shadowed_by_own_argument_test() {
  assert 1 == function_shadowed_by_own_argument(fn() { 1 })
}
