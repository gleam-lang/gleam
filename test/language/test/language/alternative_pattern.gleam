fn make_int_zero() {
  0
}

pub fn numbers_test() {
  let int = 4
  let int_2 = case int {
    1 | 2 | 3 | 4 -> 0
    _ -> 1
  }
  assert 0 == int_2
}

pub fn lists_test() {
  let ints = [1, 2]
  let int = case ints {
    [0] | [1, 2] -> 0
    _ -> 1
  }
  assert 0 == int
}

pub fn assignment_test() {
  let ints = [1, 2]
  let int = case ints {
    [x] | [_, x] -> x
    _ -> 0
  }
  assert 2 == int
}

pub fn multiple_assignment_test() {
  let ints = [1, 2, 3]
  let value = case ints {
    [x, y] | [x, y, 3] -> #(x, y)
    _ -> #(0, 0)
  }
  assert #(1, 2) == value
}

pub fn guard_test() {
  let int_two = make_int_zero() + 2
  let ints = [1, 2]
  let int = case ints {
    [x] | [_, x] if x == int_two -> x
    _ -> 0
  }
  assert 2 == int
}

pub fn guard_left_hand_side_test() {
  let int_one = make_int_zero() + 1
  let ints = [1]
  let int = case ints {
    [x] | [_, x] if x == int_one -> x
    _ -> 0
  }
  assert 1 == int
}
