import gleam/list

pub type NonEmptyList(a) {
  End(first: a)
  Next(first: a, rest: NonEmptyList(a))
}

pub fn fold(
  over list: NonEmptyList(a),
  from initial: b,
  with fun: fn(b, a) -> b,
) -> b {
  case list {
    End(item) -> fun(initial, item)
    Next(x, xs) -> fold(xs, fun(initial, x), fun)
  }
}

pub fn count(list: NonEmptyList(a)) -> Int {
  fold(list, 0, fn(acc, _) { acc + 1 })
}

pub fn map(list: NonEmptyList(a), transform: fn(a) -> b) -> NonEmptyList(b) {
  case list {
    End(x) -> End(transform(x))
    Next(x, xs) ->
      fold(xs, End(transform(x)), fn(acc, item) { Next(transform(item), acc) })
      |> reverse
  }
}

pub fn filter(list: NonEmptyList(a), predicate: fn(a) -> Bool) -> List(a) {
  fold(
    list,
    [],
    fn(acc, item) {
      case predicate(item) {
        True -> [item, ..acc]
        False -> acc
      }
    },
  )
  |> list.reverse()
}

pub fn to_list(list: NonEmptyList(a)) -> List(a) {
  fold(list, [], fn(acc, item) { [item, ..acc] })
  |> list.reverse()
}

pub fn from_list(list: List(a)) -> Result(NonEmptyList(a), Nil) {
  case list {
    [] -> Error(Nil)
    [x, ..xs] -> Ok(list.fold(xs, End(x), fn(acc, item) { Next(item, acc) }))
  }
}

pub fn reverse(list: NonEmptyList(a)) -> NonEmptyList(a) {
  case list {
    End(_) -> list
    Next(x, xs) -> fold(xs, End(x), fn(acc, x) { Next(x, acc) })
  }
}
