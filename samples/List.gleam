module List

export length/1, reverse/1, empty/1, member/2, head/1, tail/1, filter/2,
       map/2, drop/2, take/2, of/1, new/0

import Maybe exposing Maybe(..)

// Using the Erlang C BIF implementation.
//
foreign length :erlang :length :: List(a) -> Int

test length =
  length([]) |> Assert.equal(_, 0)
  length([1]) |> Assert.equal(_, 1)
  length([1, 1]) |> Assert.equal(_, 2)
  length([1, 1, 1]) |> Assert.equal(_, 3)

// Using the Erlang C BIF implementation.
//
foreign reverse :erlang :reverse :: List(a) -> List(a)

test reverse =
  length([]) |> Assert.equal(_, [])
  length([1, 2, 3, 4, 5]) |> Assert.equal(_, [5, 4, 3, 2, 1])

fn is_empty(list) =
  list == []

test is_empty =
  is_empty([]) |> Assert.true
  is_empty([1]) |> Assert.false

fn member(list, elem) =
  case list
  | [] => False
  | elem :: _ => True
  | _ :: rest => member(rest, elem)

test is_member =
  is_member([0, 4, 5, 1], 1) |> Assert.true
  is_member([0, 4, 5, 7], 1) |> Assert.false
  is_member([], 1) |> Assert.false

fn head(list) =
  case list
  | [] => Nothing
  | elem :: _ => Just(elem)

test head =
  head([0, 4, 5, 7]) |> Assert.equal(_, Just(0))
  head([]) |> Assert.equal(_, Nothing)

fn tail(list) =
  case list
  | [] => Nothing
  | _ :: rest => Just(rest)

test tail =
  tail([0, 4, 5, 7]) |> Assert.equal(_, Just([4, 5, 7]))
  tail([0]) |> Assert.equal(_, Just([]))
  tail([]) |> Assert.equal(_, Nothing)

fn filter(list, fun) =
  filter(list, fun, [])

test filter =
  filter([], |x| True) |> Assert.equal(_, [])
  filter([0, 4, 5, 7, 3], |x| True) |> Assert.equal(_, [0, 4, 5, 7, 3])
  filter([0, 4, 5, 7, 3], |x| x > 4) |> Assert.equal(_, [5, 7])
  filter([0, 4, 5, 7, 3], |x| x < 4) |> Assert.equal(_, [0, 3])

fn filter(list, fun, acc) =
  case list
  | [] => reverse(acc)
  | x :: xs => {
    new_acc = cond | fun(x) => x :: acc | True => acc
    filter(xs, fun, new_acc)
  }

fn map(list, fun) =
  map(list, fun, [])

test map =
  map([], |x| * 2) |> Assert.equal(_, [])
  map([0, 4, 5, 7, 3], |x| x * 2) |> Assert.equal(_, [0, 8, 10, 14, 6])

fn map(list, fun, acc) =
  case list
  | [] => reverse(acc)
  | x :: xs => map(xs, fun, fun(x) :: acc)

fn drop(list, n) =
  cond
  | n <= 0 => list
  | True => {
    case list
    | [] => []
    | _ :: xs => drop(xs, n - 1)
  }

test drop/2 =
  drop([], 5) |> Assert.equal(_, [])
  drop([1, 2, 3, 4, 5, 6, 7, 8], 5) |> Assert.equal(_, [6, 7, 8])

fn take(list, n) =
  take(list, n, [])

fn take(list, n, acc) =
  cond
  | n <= 0 => reverse(acc)
  | True => {
    case list
    | [] => reverse(acc)
    | x :: xs => take(xs, n - 1, x :: acc)
  }

test take =
  take([], 5) |> Assert.equal(_, [])
  take([1, 2, 3, 4, 5, 6, 7, 8], 5) |> Assert.equal(_, [1, 2, 3, 4, 5])

fn of(x) =
  [x]

test of =
  of([]) |> Assert.equal(_, [[]])
  of(1) |> Assert.equal(_, [1])

fn new() =
  []

test new =
  new() |> Assert.equal(_, [])
