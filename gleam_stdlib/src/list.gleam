import assert
import result:Result:*
import bool:Bool:*

pub enum Error =
  | Empty

import Error:*

// Using the Erlang C BIF implementation.
//
pub external fn length(List(a)) => Int = 'erlang' 'length'

test length {
  length([]) |> assert:equal(_, 0)
  length([1]) |> assert:equal(_, 1)
  length([1, 1]) |> assert:equal(_, 2)
  length([1, 1, 1]) |> assert:equal(_, 3)
}

// Using the Erlang C BIF implementation.
//
pub external fn reverse(List(a)) => List(a) = 'erlang' 'reverse'

test reverse {
  length([]) |> assert:equal(_, [])
  length([1, 2, 3, 4, 5]) |> assert:equal(_, [5, 4, 3, 2, 1])
}

pub fn is_empty(list) {
  list == []
}

test is_empty {
  is_empty([]) |> assert:true
  is_empty([1]) |> assert:false
}

pub fn member(list, elem) {
  case list {
  | [] => False
  | elem :: _ => True
  | _ :: rest => member(rest, elem)
  }
}

test is_member {
  is_member([0, 4, 5, 1], 1) |> assert:true
  is_member([0, 4, 5, 7], 1) |> assert:false
  is_member([], 1) |> assert:false
}

pub fn head(list) {
  case list {
  | [] => Error(Empty)
  | elem :: _ => Ok(elem)
  }
}

test head {
  head([0, 4, 5, 7])
    |> assert:equal(_, Ok(0))

  head([])
    |> assert:equal(_, Error(Empty))
}

pub fn tail(list) {
  case list {
  | [] => Error(Empty)
  | _ :: rest => Ok(rest)
  }
}

test tail {
  tail([0, 4, 5, 7])
    |> assert:equal(_, Ok([4, 5, 7]))

  tail([0])
    |> assert:equal(_, Ok([]))

  tail([])
    |> assert:equal(_, Error(Empty))
}

pub fn filter(list, fun) {
  filter(list, fun, [])
}

test filter {
  []
    |> filter(_, fn(x) { True })
    |> assert:equal(_, [])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { True })
    |> assert:equal(_, [0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { x > 4 })
    |> assert:equal(_, [5, 7])

  [0, 4, 5, 7, 3]
    |> filter(_, fn(x) { x < 4 })
    |> assert:equal(_, [0, 3])
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
    |> assert:equal(_, [])

  [0, 4, 5, 7, 3]
    |> map(_, fn(x) { x * 2 })
    |> assert:equal(_, [0, 8, 10, 14, 6])
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
    |> assert:equal(_, [])

  [1, 2, 3, 4, 5, 6, 7, 8]
    |> drop(_, 5)
    |> assert:equal(_, [6, 7, 8])
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
    |> assert:equal(_, [])
  [1, 2, 3, 4, 5, 6, 7, 8]
    |> take(_, 5)
    |> assert:equal(_, [1, 2, 3, 4, 5])
}

pub fn of(x) {
  [x]
}

test of() {
  of([]) |> assert:equal(_, [[]])
  of(1) |> assert:equal(_, [1])
}

pub fn new() {
  []
}

test new() {
  new() |> assert:equal(_, [])
}

pub fn flatten(lists) {
  do_flatten(lists, [])
}

test flatten() {
  flatten([])
    |> assert:equal(_, [])

  flatten([[]])
    |> assert:equal(_, [])

  flatten([[], [], []])
    |> assert:equal(_, [])

  flatten([[1, 2], [], [3, 4]])
    |> assert:equal(_, [1, 2, 3, 4])
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
    |> assert:equal(_, [3, 2, 1])
}

pub fn foldr(list, acc, fun) {
  case list {
  | [] => acc
  | x :: rest => fun(x, foldl(rest, acc, fun))
  }
}

test foldr() {
  [1, 2, 3]
    |> foldr(_, [], fn(x, acc) { x :: acc })
    |> assert:equal(_, [1, 2, 3])
}
