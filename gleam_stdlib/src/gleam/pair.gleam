// import gleam/list

pub struct Pair(a, b) {
  first: a
  second: b
}

pub fn first(tup) {
  let Pair(a, _) = tup
  a
}

pub fn second(tup) {
  let Pair(_, a) = tup
  a
}

pub fn swap(tup) {
  let Pair(a, b) = tup
  Pair(b, a)
}

// pub fn fetch(haystack, needle) {
//   list.find(haystack, fn(tuple) {
//     case first(tuple) == needle {
//     | True -> tuple |> second |> Ok
//     | False -> Error([])
//     }
//   })
// }
