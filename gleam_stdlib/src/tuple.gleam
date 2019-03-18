import expect

pub fn first(tup) {
  let {a, _} = tup
  a
}

test first {
  {1, 2}
  |> first
  |> expect:equal(_, 1)
}

pub fn second(tup) {
  let {_, a} = tup
  a
}

test second {
  {1, 2}
  |> second
  |> expect:equal(_, 2)
}
