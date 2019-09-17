pub struct Triple(a, b, c) {
  first: a
  second: b
  third: c
}

pub fn first(trip) {
  let Triple(a, _, _) = trip
  a
}

pub fn second(trip) {
  let Triple(_, a, _) = trip
  a
}

pub fn third(trip) {
  let Triple(_, _, a) = trip
  a
}
