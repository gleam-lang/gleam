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

pub fn ten_million_recursions_doesnt_overflow_the_stack_test() {
  assert Nil == count_down(from: 10_000_000)
}

// https://github.com/gleam-lang/gleam/issues/1214
// https://github.com/gleam-lang/gleam/issues/1380
pub fn arguments_correctly_reassigned_test() {
  assert [1, 2, 3] == tail_recursive_accumulate_down(3, [])
}

// https://github.com/gleam-lang/gleam/issues/2400
pub fn function_shadowed_by_own_argument_test() {
  assert 1 == function_shadowed_by_own_argument(fn() { 1 })
}
