import gleam/list

pub fn new(a, b) {
  struct(a, b)
}

pub fn first(tup) {
  let struct(a, _) = tup
  a
}

pub fn second(tup) {
  let struct(_, a) = tup
  a
}

pub fn swap(tup) {
  let struct(a, b) = tup
  struct(b, a)
}

pub fn fetch(haystack, needle) {
  list:find(haystack, fn(tuple) {
    case first(tuple) == needle {
    | True -> tuple |> second |> Ok
    | False -> Error([])
    }
  })
}
