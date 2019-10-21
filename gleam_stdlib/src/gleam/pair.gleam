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

pub fn map_first(of tup, with fun) {
    let Pair(a, b) = tup
    Pair(fun(a), b)
}

pub fn map_second(of tup, with fun) {
    let Pair(a, b) = tup
    Pair(a, fun(b))
}
