module List

import Result:Result:*

pub type Err =
  | Empty

// Using the Erlang C BIF implementation.
//
external length : fn(List(a)) { Int } = :erlang :length

test length {
  length([]) |> Assert.equal(_, 0)
  length([1]) |> Assert.equal(_, 1)
  length([1, 1]) |> Assert.equal(_, 2)
  length([1, 1, 1]) |> Assert.equal(_, 3)
}

// Using the Erlang C BIF implementation.
//
pub external fn reverse(List(a)) { List(a) } = :erlang :reverse

test reverse {
  length([]) |> Assert.equal(_, [])
  length([1, 2, 3, 4, 5]) |> Assert.equal(_, [5, 4, 3, 2, 1])
}

pub fn is_empty(list) {
  list == []
}

test is_empty {
  is_empty([]) |> Assert.true
  is_empty([1]) |> Assert.false
}

pub fn member(list, elem) {
  case list {
  | [] => False
  | elem :: _ => True
  | _ :: rest => member(rest, elem)
  }
}

test is_member {
  is_member([0, 4, 5, 1], 1) |> Assert.true
  is_member([0, 4, 5, 7], 1) |> Assert.false
  is_member([], 1) |> Assert.false
}

pub fn head(list) {
  case list {
  | [] => Error(Err:Empty)
  | elem :: _ => Ok(elem)
  }
}

test head {
  head([0, 4, 5, 7])
    |> Assert.equal(_, Ok(0))

  head([])
    |> Assert.equal(_, Error(Err:Empty))
}

pub fn tail(list) {
  case list {
  | [] => Error(Err:Empty)
  | _ :: rest => Ok(rest)
  }
}

test tail {
  tail([0, 4, 5, 7])
    |> Assert.equal(_, Ok([4, 5, 7]))

  tail([0])
    |> Assert.equal(_, Ok([]))

  tail([])
    |> Assert.equal(_, Error(Err:Empty))
}

pub fn filter(list, fun) {
  filter(list, fun, [])
}

test filter {
  []
    |> filter(_, fn(x) { True })
    |> Assert.equal(_, [])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { True })
    |> Assert.equal(_, [0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { x > 4 })
    |> Assert.equal(_, [5, 7])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { x < 4 })
    |> Assert.equal(_, [0, 3])
}

fn do_filter(list, fun, acc) {
  case list {
  | [] => reverse(acc)
  | x :: xs =>
      new_acc =
        case fun(x) {
        | True => x :: acc
        | False => acc
        }
      do_filter(xs, fun, new_acc)
  }
}

pub fn map(list, fun) {
  do_map(list, fun, [])
}

test map {
  []
    |> map(_, fn(x) { x * 2 })
    |> Assert.equal(_, [])

  [0, 4, 5, 7, 3]
    |> map(_, fn(x) { x * 2 })
    |> Assert.equal(_, [0, 8, 10, 14, 6])
}

fn do_map(list, fun, acc) {
  case list {
  | [] => reverse(acc)
  | x :: xs => do_map(xs, fun, fun(x) :: acc)
  }
}

pub fn drop(list, n) {
  case n <= 0 {
  | True => list
  | False =>
      case list {
      | [] => []
      | _ :: xs => drop(xs, n - 1)
      }
  }
}

test drop/2 {
  []
    |> drop(_, 5)
    |> Assert.equal(_, [])
  [1, 2, 3, 4, 5, 6, 7, 8]
    |> drop(_, 5)
    |> Assert.equal(_, [6, 7, 8])
}

pub fn take(list, n) {
  do_take(list, n, [])
}

fn do_take(list, n, acc) {
  case n <= 0 {
  | True => reverse(acc)
  | False =>
      case list {
      | [] => reverse(acc)
      | x :: xs => take(xs, n - 1, x :: acc)
      }
  }
}

test take {
  []
    |> take(_, 5)
    |> Assert.equal(_, [])
  [1, 2, 3, 4, 5, 6, 7, 8]
    |> take(_, 5)
    |> Assert.equal(_, [1, 2, 3, 4, 5])
}

pub fn of(x) {
  [x]
}

test of() {
  of([]) |> Assert.equal(_, [[]])
  of(1) |> Assert.equal(_, [1])
}

pub fn new() {
  []
}

test new() {
  new() |> Assert.equal(_, [])
}

pub fn flatten(lists) {
  do_flatten(lists, [])
}

test flatten() {
  flatten([]) |> Assert.equal(_, [])
  flatten([[]]) |> Assert.equal(_, [])
  flatten([[], [], []]) |> Assert.equal(_, [])
  flatten([[1, 2], [], [3, 4]]) |> Assert.equal(_, [1, 2, 3, 4])
}

fn do_flatten(lists, acc) {
  case lists {
  | [] => acc
  | l :: rest => flatten(rest, acc ++ l)
  }
}

pub fn foldl(list, acc, fun) {
  case list {
  | [] => acc
  | x :: rest => foldl(rest, fun(x, acc), fun)
  }
}

test foldl() {
  [1, 2, 3]
    |> foldl(_, [], fn(x, acc) { x :: acc })
    |> Assert.equal(_, [3, 2, 1])
}

pub fn foldr(list, acc, fun) {
  case list {
  | [] => acc
  | x :: rest => fun(x, foldl(rest, acc, fun))
  }
}

test foldr() {
  [1, 2, 3]
    |> foldr(_, [], |x, acc| x :: acc)
    |> Assert.equal(_, [1, 2, 3])
}
