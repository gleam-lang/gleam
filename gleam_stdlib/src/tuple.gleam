import expect
import result
import list

pub fn new(a, b) {
  {a, b}
}

test new {
  new(1, 2)
  |> expect:equal(_, {1, 2})

  new(2, "3")
  |> expect:equal(_, {2, "3"})
}

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

pub fn swap(tup) {
  let {a, b} = tup
  {b, a}
}

test swap {
  {1, "2"}
  |> swap
  |> expect:equal(_, {"2", 1})
}

pub fn fetch(haystack, needle) {
  list:find(haystack, fn(tuple) {
    case first(tuple) == needle {
    | True -> Ok(second(tuple))
    | False -> Error([])
    }
  })
}

test fetch {
  let proplist = [{0, "1"}, {1, "2"}]

  proplist
  |> fetch(_, 0)
  |> expect:equal(_, Ok("1"))

  proplist
  |> fetch(_, 1)
  |> expect:equal(_, Ok("2"))

  proplist
  |> fetch(_, 2)
  |> expect:is_error
}
