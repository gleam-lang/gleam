fn identity(x: a) -> a {
  x
}

fn pair(x: a, y: b) -> #(a, b) {
  #(x, y)
}

fn triplet(x x: a, y y: b, z z: c) -> #(a, b, c) {
  #(x, y, z)
}

pub fn pipe_last_test() {
  let result =
    100
    |> identity
  assert 100 == result
}

pub fn pipe_into_anon_test() {
  let result =
    100
    |> fn(x) { x }
  assert 100 == result
}

pub fn pipe_into_capture_test() {
  let result =
    1
    |> pair(2, _)
  assert #(2, 1) == result
}

pub fn pipe_first_test() {
  let result =
    1
    |> pair(2)
  assert #(1, 2) == result
}

pub fn pipe_middle_with_label_requires_false_capture_test() {
  let result =
    2
    |> triplet(z: 3, x: 1)
  assert #(1, 2, 3) == result
}

pub fn pipe_last_with_label_requires_false_capture_test() {
  let result =
    3
    |> triplet(y: 2, x: 1)
  assert #(1, 2, 3) == result
}
