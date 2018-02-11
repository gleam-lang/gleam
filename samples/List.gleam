module List

export length/1, reverse/1, empty?/1, member?/2, head/1, tail/1, filter/2,
       map/2, drop/2, take/2, of/1, new/0

import Maybe exposing Maybe(..)

// Using the Erlang C BIF implementation.
//
foreign length :erlang :length :: List(a) -> Int

test "length/1" =
  length([]) |> Assert.equal(_, 0)
  length([1]) |> Assert.equal(_, 1)
  length([1, 1]) |> Assert.equal(_, 2)
  length([1, 1, 1]) |> Assert.equal(_, 3)

// Using the Erlang C BIF implementation.
//
foreign reverse :erlang :reverse :: List(a) -> List(a)

test "reverse/1" =
  length([]) |> Assert.equal(_, [])
  length([1, 2, 3, 4, 5]) |> Assert.equal(_, [5, 4, 3, 2, 1])

let empty?(list) =
  list == []

test "empty?/1" =
  empty?([]) |> Assert.true
  empty?([1]) |> Assert.false

let member?(list, elem) =
  match list
  | [] => False
  | elem :: _ => True
  | _ :: rest => member?(rest, elem)

test "member?/1" =
  member?([0, 4, 5, 1], 1) |> Assert.true
  member?([0, 4, 5, 7], 1) |> Assert.false
  member?([], 1) |> Assert.false

let head(list) =
  match list
  | [] => Nothing
  | elem :: _ => Just(elem)

test "head/1" =
  head([0, 4, 5, 7]) |> Assert.equal(_, Just(0))
  head([]) |> Assert.equal(_, Nothing)

let tail(list) =
  match list
  | [] => Nothing
  | _ :: rest => Just(rest)

test "tail/1" =
  tail([0, 4, 5, 7]) |> Assert.equal(_, Just([4, 5, 7]))
  tail([0]) |> Assert.equal(_, Just([]))
  tail([]) |> Assert.equal(_, Nothing)

let filter(list, fun) =
  filter(list, fun, [])

test "filter/2" =
  filter([], |x| True) |> Assert.equal(_, [])
  filter([0, 4, 5, 7, 3], |x| True) |> Assert.equal(_, [0, 4, 5, 7, 3])
  filter([0, 4, 5, 7, 3], |x| x > 4) |> Assert.equal(_, [5, 7])
  filter([0, 4, 5, 7, 3], |x| x < 4) |> Assert.equal(_, [0, 3])

let filter(list, fun, acc) =
  match list
  | [] => reverse(acc)
  | x :: xs => {
    new_acc = cond | fun(x) => x :: acc | True => acc
    filter(xs, fun, new_acc)
  }

let map(list, fun) =
  map(list, fun, [])

test "map/2" =
  map([], |x| * 2) |> Assert.equal(_, [])
  map([0, 4, 5, 7, 3], |x| x * 2) |> Assert.equal(_, [0, 8, 10, 14, 6])

let map(list, fun, acc) =
  match list
  | [] => reverse(acc)
  | x :: xs => map(xs, fun, fun(x) :: acc)

let drop(list, n) =
  cond
  | n <= 0 => list
  | True => {
    match list
    | [] => []
    | _ :: xs => drop(xs, n - 1)
  }

test "drop/2" =
  drop([], 5) |> Assert.equal(_, [])
  drop([1, 2, 3, 4, 5, 6, 7, 8], 5) |> Assert.equal(_, [6, 7, 8])

let take(list, n) =
  take(list, n, [])

let take(list, n, acc) =
  cond
  | n <= 0 => reverse(acc)
  | True => {
    match list
    | [] => reverse(acc)
    | x :: xs => take(xs, n - 1, x :: acc)
  }

test "take/2" =
  take([], 5) |> Assert.equal(_, [])
  take([1, 2, 3, 4, 5, 6, 7, 8], 5) |> Assert.equal(_, [1, 2, 3, 4, 5])

let of(x) =
  [x]

test "of/1" =
  of([]) |> Assert.equal(_, [[]])
  of(1) |> Assert.equal(_, [1])

let new() =
  []

test "new/1" =
  new() |> Assert.equal(_, [])
