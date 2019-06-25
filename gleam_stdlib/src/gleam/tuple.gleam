import gleam/list

pub fn new(a, b) {
  {a, b}
}

pub fn first(tup) {
  let {a, _} = tup
  a
}

pub fn second(tup) {
  let {_, a} = tup
  a
}

pub fn swap(tup) {
  let {a, b} = tup
  {b, a}
}

pub fn fetch(haystack, needle) {
  list:find(haystack, fn(tuple) {
    case first(tuple) == needle {
    | True -> tuple |> second |> Ok
    | False -> Error([])
    }
  })
}
