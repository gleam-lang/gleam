import expect

pub enum Error =
  | Empty

// Using the Erlang C BIF implementation.
//
pub external fn length(List(a)) -> Int = "erlang" "length"

test length {
  length([]) |> expect:equal(_, 0)
  length([1]) |> expect:equal(_, 1)
  length([1, 1]) |> expect:equal(_, 2)
  length([1, 1, 1]) |> expect:equal(_, 3)
}

// Using the Erlang C BIF implementation.
//
pub external fn reverse(List(a)) -> List(a) = "lists" "reverse"

test reverse {
  length([]) |> expect:equal(_, 0)
  length([1, 2, 3, 4, 5]) |> expect:equal(_, 5)
}

pub fn is_empty(list) {
  list == []
}

test is_empty {
  is_empty([]) |> expect:true
  is_empty([1]) |> expect:false
}

pub fn has_member(list, elem) {
  case list {
  | [] -> False
  | [head | rest] -> head == elem || has_member(rest, elem)
  }
}

test has_member {
  has_member([0, 4, 5, 1], 1) |> expect:true
  has_member([0, 4, 5, 7], 1) |> expect:false
  has_member([], 1) |> expect:false
}

pub fn head(list) {
  case list {
  | [] -> Error(Empty)
  | [x | _] -> Ok(x)
  }
}

test head {
  head([0, 4, 5, 7])
    |> expect:equal(_, Ok(0))

  head([])
    |> expect:equal(_, Error(Empty))
}

pub fn tail(list) {
  case list {
  | [] -> Error(Empty)
  | [_ | xs] -> Ok(xs)
  }
}

test tail {
  tail([0, 4, 5, 7])
    |> expect:equal(_, Ok([4, 5, 7]))

  tail([0])
    |> expect:equal(_, Ok([]))

  tail([])
    |> expect:equal(_, Error(Empty))
}

fn do_filter(list, fun, acc) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] ->
      let new_acc =
        case fun(x) {
        | True -> [x | acc]
        | False -> acc
        }
      do_filter(xs, fun, new_acc)
  }
}

pub fn filter(list, fun) {
  do_filter(list, fun, [])
}

test filter {
  []
    |> filter(_, fn(x) { True })
    |> expect:equal(_, [])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { True })
    |> expect:equal(_, [0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { x > 4 })
    |> expect:equal(_, [5, 7])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { x < 4 })
    |> expect:equal(_, [0, 3])
}

fn do_map(list, fun, acc) {
  case list {
  | [] -> reverse(acc)
  | [x | xs] -> do_map(xs, fun, [fun(x) | acc])
  }
}

pub fn map(list, fun) {
  do_map(list, fun, [])
}

test map {
  []
    |> map(_, fn(x) { x * 2 })
    |> expect:equal(_, [])

  [0, 4, 5, 7, 3]
    |> map(_, fn(x) { x * 2 })
    |> expect:equal(_, [0, 8, 10, 14, 6])
}

fn do_traverse(list, fun, acc) {
  case list {
  | [] -> Ok(reverse(acc))
  | [x | xs] ->
      case fun(x) {
      | Ok(y) -> do_traverse(xs, fun, [y | acc])
      | Error(error) -> Error(error)
      }
  }
}

pub fn traverse(list, fun) {
  do_traverse(list, fun, [])
}

test traverse {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
    | True -> Ok(x * 2)
    | False -> Error(x)
    }
  }

  [5, 6, 5, 6]
    |> traverse(_, fun)
    |> expect:equal(_, Ok([10, 12, 10, 12]))

  [4, 6, 5, 7, 3]
    |> traverse(_, fun)
    |> expect:equal(_, Error(7))
}


pub fn drop(list, n) {
  case n <= 0 {
  | True -> list
  | False ->
      case list {
      | [] -> []
      | [x | xs] -> drop(xs, n - 1)
      }
  }
}

test drop {
  []
    |> drop(_, 5)
    |> expect:equal(_, [])

  [1, 2, 3, 4, 5, 6, 7, 8]
    |> drop(_, 5)
    |> expect:equal(_, [6, 7, 8])
}

fn do_take(list, n, acc) {
  case n <= 0 {
  | True -> reverse(acc)
  | False ->
      case list {
      | [] -> reverse(acc)
      | [x | xs] -> do_take(xs, n - 1, [x | acc])
      }
  }
}

pub fn take(list, n) {
  do_take(list, n, [])
}

test take {
  []
    |> take(_, 5)
    |> expect:equal(_, [])
  [1, 2, 3, 4, 5, 6, 7, 8]
    |> take(_, 5)
    |> expect:equal(_, [1, 2, 3, 4, 5])
}

pub fn new() {
  []
}

test new {
  new() |> expect:equal(_, [])
}

pub external fn append(List(a), List(a)) -> List(a) = "lists" "append";

test append {
  expect:equal(
    append([1], [2, 3]),
    [1, 2, 3],
  )
}

fn do_flatten(lists, acc) {
  case lists {
  | [] -> acc
  | [l | rest] -> do_flatten(rest, append(acc, l))
  }
}

pub fn flatten(lists) {
  do_flatten(lists, [])
}

test flatten {
  flatten([])
    |> expect:equal(_, [])

  flatten([[]])
    |> expect:equal(_, [])

  flatten([[], [], []])
    |> expect:equal(_, [])

  flatten([[1, 2], [], [3, 4]])
    |> expect:equal(_, [1, 2, 3, 4])
}

pub fn foldl(list, acc, fun) {
  case list {
  | [] -> acc
  | [x | rest] -> foldl(rest, fun(x, acc), fun)
  }
}

test foldl {
  [1, 2, 3]
    |> foldl(_, [], fn(x, acc) { [x | acc] })
    |> expect:equal(_, [3, 2, 1])
}

pub fn foldr(list, acc, fun) {
  case list {
  | [] -> acc
  | [x | rest] -> fun(x, foldr(rest, acc, fun))
  }
}

test foldr {
  [1, 2, 3]
    |> foldr(_, [], fn(x, acc) { [x | acc] })
    |> expect:equal(_, [1, 2, 3])
}
