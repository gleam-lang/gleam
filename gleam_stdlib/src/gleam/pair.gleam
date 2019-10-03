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

pub fn map_first(tup, f) {
    let Pair(a, b) = tup
    Pair(f(a), b)
}

pub fn map_second(tup, f) {
    let Pair(a, b) = tup
    Pair(a, f(b))
}
