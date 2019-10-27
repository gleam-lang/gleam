import gleam/expect
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/pair.{Pair}

pub fn length_test() {
  list.length([])
  |> expect.equal(_, 0)

  list.length([1])
  |> expect.equal(_, 1)

  list.length([1, 1])
  |> expect.equal(_, 2)

  list.length([1, 1, 1])
  |> expect.equal(_, 3)
}

pub fn reverse_test() {
  list.reverse([]) |> expect.equal(_, [])
  list.reverse([1, 2, 3, 4, 5]) |> expect.equal(_, [5, 4, 3, 2, 1])
}

pub fn is_empty_test() {
  list.is_empty([]) |> expect.true
  list.is_empty([1]) |> expect.false
}

pub fn contains_test() {
  list.contains([0, 4, 5, 1], 1) |> expect.true
  list.contains([0, 4, 5, 7], 1) |> expect.false
  list.contains([], 1) |> expect.false
}

pub fn head_test() {
  list.head([0, 4, 5, 7])
    |> expect.equal(_, Ok(0))

  list.head([])
    |> expect.equal(_, Error(Nil))
}

pub fn tail_test() {
  list.tail([0, 4, 5, 7])
    |> expect.equal(_, Ok([4, 5, 7]))

  list.tail([0])
    |> expect.equal(_, Ok([]))

  list.tail([])
    |> expect.equal(_, Error(Nil))
}

pub fn filter_test() {
  []
    |> list.filter(_, fn(_) { True })
    |> expect.equal(_, [])

  [0, 4, 5, 7, 3]
    |> list.filter(_, fn(_) { True })
    |> expect.equal(_, [0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
    |> list.filter(_, fn(x) { x > 4 })
    |> expect.equal(_, [5, 7])

  [0, 4, 5, 7, 3]
    |> list.filter(_, fn(x) { x < 4 })
    |> expect.equal(_, [0, 3])
}

pub fn map_test() {
  []
    |> list.map(_, fn(x) { x * 2 })
    |> expect.equal(_, [])

  [0, 4, 5, 7, 3]
    |> list.map(_, fn(x) { x * 2 })
    |> expect.equal(_, [0, 8, 10, 14, 6])
}

pub fn traverse_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
    | True -> Ok(x * 2)
    | False -> Error(x)
    }
  }

  [5, 6, 5, 6]
    |> list.traverse(_, fun)
    |> expect.equal(_, Ok([10, 12, 10, 12]))

  [4, 6, 5, 7, 3]
    |> list.traverse(_, fun)
    |> expect.equal(_, Error(7))
}

pub fn drop_test() {
  []
    |> list.drop(_, 5)
    |> expect.equal(_, [])

  [1, 2, 3, 4, 5, 6, 7, 8]
    |> list.drop(_, 5)
    |> expect.equal(_, [6, 7, 8])
}

pub fn take_test() {
  []
    |> list.take(_, 5)
    |> expect.equal(_, [])
  [1, 2, 3, 4, 5, 6, 7, 8]
    |> list.take(_, 5)
    |> expect.equal(_, [1, 2, 3, 4, 5])
}

pub fn new_test() {
  list.new() |> expect.equal(_, [])
}

pub fn append_test() {
  list.append([1], [2, 3])
    |> expect.equal(_, [1, 2, 3])
}

pub fn flatten_test() {
  list.flatten([])
    |> expect.equal(_, [])

  list.flatten([[]])
    |> expect.equal(_, [])

  list.flatten([[], [], []])
    |> expect.equal(_, [])

  list.flatten([[1, 2], [], [3, 4]])
    |> expect.equal(_, [1, 2, 3, 4])
}

pub fn fold_test() {
  [1, 2, 3]
    |> list.fold(_, [], fn(x, acc) { [x | acc] })
    |> expect.equal(_, [3, 2, 1])
}

pub fn fold_right_test() {
  [1, 2, 3]
  |> list.fold_right(_, from: [], with: fn(x, acc) { [x | acc] })
  |> expect.equal(_, [1, 2, 3])
}

pub fn find_map_test() {
  let f = fn(x) {
    case x {
    | 2 -> Ok(4)
    | _ -> Error(0)
    }
  }

  [1, 2, 3]
  |> list.find_map(_, with: f)
  |> expect.equal(_, Ok(4))

  [1, 3, 2]
  |> list.find_map(_, with: f)
  |> expect.equal(_, Ok(4))

  [1, 3]
  |> list.find_map(_, with: f)
  |> expect.equal(_, Error(Nil))
}

pub fn find_test() {
  let is_two = fn(x) {
    x == 2
  }

  [1, 2, 3]
  |> list.find(_, one_that: is_two)
  |> expect.equal(_, Ok(2))

  [1, 3, 2]
  |> list.find(_, one_that: is_two)
  |> expect.equal(_, Ok(2))

  [1, 3]
  |> list.find(_, one_that: is_two)
  |> expect.equal(_, Error(Nil))
}

pub fn all_test() {
  list.all([1, 2, 3, 4, 5], fn(x) { x > 0 })
  |> expect.equal(_, True)

  list.all([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> expect.equal(_, False)

  list.all([], fn(_) { False })
  |> expect.equal(_, True)
}

pub fn any_test() {
  list.any([1, 2, 3, 4, 5], fn(x) { x == 2 })
  |> expect.equal(_, True)

  list.any([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> expect.equal(_, False)

  list.any([], fn(_) { False })
  |> expect.equal(_, False)
}

pub fn zip_test() {
  list.zip([], [1, 2, 3])
  |> expect.equal(_, [])

  list.zip([1, 2], [])
  |> expect.equal(_, [])

  list.zip([1, 2, 3], [4, 5, 6])
  |> expect.equal(_, [Pair(1, 4), Pair(2, 5), Pair(3, 6)])

  list.zip([5, 6], [1, 2, 3])
  |> expect.equal(_, [Pair(5, 1), Pair(6, 2)])

  list.zip([5, 6, 7], [1, 2])
  |> expect.equal(_, [Pair(5, 1), Pair(6, 2)])
}

pub fn strict_zip_test() {
  list.strict_zip([], [1, 2, 3])
  |> expect.equal(_, Error(list.LengthMismatch))

  list.strict_zip([1, 2], [])
  |> expect.equal(_, Error(list.LengthMismatch))

  list.strict_zip([1, 2, 3], [4, 5, 6])
  |> expect.equal(_, Ok([
    Pair(1, 4),
    Pair(2, 5),
    Pair(3, 6),
  ]))

  list.strict_zip([5, 6], [1, 2, 3])
  |> expect.equal(_, Error(list.LengthMismatch))

  list.strict_zip([5, 6, 7], [1, 2])
  |> expect.equal(_, Error(list.LengthMismatch))
}

pub fn intersperse_test() {
  list.intersperse([1, 2, 3], 4)
  |> expect.equal(_, [1, 4, 2, 4, 3])

  list.intersperse([], 2)
  |> expect.equal(_, [])
}

pub fn at_test() {
  list.at([1, 2, 3], 2)
  |> expect.equal(_, Ok(3))

  list.at([1, 2, 3], 5)
  |> expect.equal(_, Error(Nil))

  list.at([], 0)
  |> expect.equal(_, Error(Nil))

  list.at([1, 2, 3, 4, 5, 6], -1)
  |> expect.equal(_, Error(Nil))
}

pub fn unique_test() {
  list.unique([1, 1, 2, 3, 4, 4, 4, 5, 6])
  |> expect.equal(_, [1, 2, 3, 4, 5, 6])

  list.unique([7, 1, 45, 6, 2, 47, 2, 7, 5])
  |> expect.equal(_, [7, 1, 45, 6, 2, 47, 5])

  list.unique([3, 4, 5])
  |> expect.equal(_, [3, 4, 5])

  list.unique([])
  |> expect.equal(_, [])
}

pub fn sort_test() {
  [4, 3, 6, 5, 4]
  |> list.sort(_, int.compare)
  |> expect.equal(_, [3, 4, 4, 5, 6])

  [4, 3, 6, 5, 4, 1]
  |> list.sort(_, int.compare)
  |> expect.equal(_, [1, 3, 4, 4, 5, 6])

  [4.1, 3.1, 6.1, 5.1, 4.1]
  |> list.sort(_, float.compare)
  |> expect.equal(_, [3.1, 4.1, 4.1, 5.1, 6.1])

  []
  |> list.sort(_, int.compare)
  |> expect.equal(_, [])
}

pub fn index_map_test() {
  list.index_map([3, 4, 5], fn(i, x) { Pair(i, x) })
  |> expect.equal(_, [Pair(0, 3), Pair(1, 4), Pair(2, 5)])

  let f = fn(i, x) {
    string.append(x, int.to_string(i))
  }
  list.index_map(["a", "b", "c"], f)
  |> expect.equal(_, ["a0", "b1", "c2"])
}

pub fn range_test() {
  list.range(0, 0)
  |> expect.equal(_, [])

  list.range(1, 1)
  |> expect.equal(_, [])

  list.range(-1, -1)
  |> expect.equal(_, [])

  list.range(0, 1)
  |> expect.equal(_, [0])

  list.range(0, 5)
  |> expect.equal(_, [0, 1, 2, 3, 4])

  list.range(1, -5)
  |> expect.equal(_, [1, 0, -1, -2, -3, -4])
}

pub fn repeat_test() {
  list.repeat(1, -10)
  |> expect.equal(_, [])

  list.repeat(1, 0)
  |> expect.equal(_, [])

  list.repeat(2, 3)
  |> expect.equal(_, [2, 2, 2])

  list.repeat("x", 5)
  |> expect.equal(_, ["x", "x", "x", "x", "x"])
}

pub fn split_test() {
  list.split([], 0)
  |> expect.equal(_, Pair([], []))

  list.split([0, 1, 2, 3, 4], 0)
  |> expect.equal(_, Pair([], [0, 1, 2, 3, 4]))

  list.split([0, 1, 2, 3, 4], -2)
  |> expect.equal(_, Pair([], [0, 1, 2, 3, 4]))

  list.split([0, 1, 2, 3, 4], 1)
  |> expect.equal(_, Pair([0], [1, 2, 3, 4]))

  list.split([0, 1, 2, 3, 4], 3)
  |> expect.equal(_, Pair([0, 1, 2], [3, 4]))

  list.split([0, 1, 2, 3, 4], 9)
  |> expect.equal(_, Pair([0, 1, 2, 3, 4], []))
}

pub fn split_while_test() {
  list.split_while([], fn(x) { x <= 5 })
  |> expect.equal(_, Pair([], []))

  list.split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })
  |> expect.equal(_, Pair([1, 2, 3, 4, 5], []))

  list.split_while([1, 2, 3, 4, 5], fn(x) { x == 2 })
  |> expect.equal(_, Pair([], [1, 2, 3, 4, 5]))

  list.split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })
  |> expect.equal(_, Pair([1, 2, 3], [4, 5]))

  list.split_while([1, 2, 3, 4, 5], fn(x) { x <= -3 })
  |> expect.equal(_, Pair([], [1, 2, 3, 4, 5]))
}


pub fn key_find_test() {
  let proplist = [Pair(0, "1"), Pair(1, "2")]

  proplist
  |> list.key_find(_, 0)
  |> expect.equal(_, Ok("1"))

  proplist
  |> list.key_find(_, 1)
  |> expect.equal(_, Ok("2"))

  proplist
  |> list.key_find(_, 2)
  |> expect.equal(_, Error(Nil))
}
