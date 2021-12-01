import gleam/should
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/pair

pub fn length_test() {
  list.length([])
  |> should.equal(0)

  list.length([1])
  |> should.equal(1)

  list.length([1, 1])
  |> should.equal(2)

  list.length([1, 1, 1])
  |> should.equal(3)
}

pub fn reverse_test() {
  list.reverse([])
  |> should.equal([])

  list.reverse([1])
  |> should.equal([1])

  list.reverse([1, 2])
  |> should.equal([2, 1])

  list.reverse([1, 2, 3, 4, 5])
  |> should.equal([5, 4, 3, 2, 1])
}

pub fn is_empty_test() {
  list.is_empty([])
  |> should.be_true
  list.is_empty([1])
  |> should.be_false
}

pub fn contains_test() {
  list.contains([0, 4, 5, 1], 1)
  |> should.be_true
  list.contains([0, 4, 5, 7], 1)
  |> should.be_false
  list.contains([], 1)
  |> should.be_false
}

pub fn first_test() {
  list.first([0, 4, 5, 7])
  |> should.equal(Ok(0))

  list.first([])
  |> should.equal(Error(Nil))
}

pub fn rest_test() {
  list.rest([0, 4, 5, 7])
  |> should.equal(Ok([4, 5, 7]))

  list.rest([0])
  |> should.equal(Ok([]))

  list.rest([])
  |> should.equal(Error(Nil))
}

pub fn filter_test() {
  []
  |> list.filter(fn(_) { True })
  |> should.equal([])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(_) { True })
  |> should.equal([0, 4, 5, 7, 3])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(x) { x > 4 })
  |> should.equal([5, 7])

  [0, 4, 5, 7, 3]
  |> list.filter(fn(x) { x < 4 })
  |> should.equal([0, 3])
}

pub fn filter_map_test() {
  [2, 4, 6, 1]
  |> list.filter_map(fn(x) { Ok(x + 1) })
  |> should.equal([3, 5, 7, 2])

  [2, 4, 6, 1]
  |> list.filter_map(Error)
  |> should.equal([])
}

pub fn map_test() {
  []
  |> list.map(fn(x) { x * 2 })
  |> should.equal([])

  [0, 4, 5, 7, 3]
  |> list.map(fn(x) { x * 2 })
  |> should.equal([0, 8, 10, 14, 6])
}

pub fn map_fold_test() {
  [1, 2, 3, 4]
  |> list.map_fold(from: 0, with: fn(acc, i) { #(acc + i, i * 2) })
  |> should.equal(#(10, [2, 4, 6, 8]))
}

pub fn try_map_test() {
  let fun = fn(x) {
    case x == 6 || x == 5 || x == 4 {
      True -> Ok(x * 2)
      False -> Error(x)
    }
  }

  [5, 6, 5, 6]
  |> list.try_map(fun)
  |> should.equal(Ok([10, 12, 10, 12]))

  [4, 6, 5, 7, 3]
  |> list.try_map(fun)
  |> should.equal(Error(7))
}

pub fn drop_test() {
  []
  |> list.drop(5)
  |> should.equal([])

  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.drop(5)
  |> should.equal([6, 7, 8])
}

pub fn take_test() {
  []
  |> list.take(5)
  |> should.equal([])
  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.take(5)
  |> should.equal([1, 2, 3, 4, 5])
}

pub fn new_test() {
  list.new()
  |> should.equal([])
}

pub fn append_test() {
  list.append([1], [2, 3])
  |> should.equal([1, 2, 3])

  list.append([1, 2], [])
  |> should.equal([1, 2])

  list.append([], [1, 2])
  |> should.equal([1, 2])

  list.append([1, 2], [3, 4])
  |> should.equal([1, 2, 3, 4])

  list.append([1, 2, 3], [])
  |> should.equal([1, 2, 3])

  list.append([1, 2, 3], [4])
  |> should.equal([1, 2, 3, 4])

  list.append([1, 2, 3, 4], [5])
  |> should.equal([1, 2, 3, 4, 5])

  list.append([], [])
  |> should.equal([])
}

pub fn flatten_test() {
  list.flatten([])
  |> should.equal([])

  list.flatten([[]])
  |> should.equal([])

  list.flatten([[], [], []])
  |> should.equal([])

  list.flatten([[1, 2], [], [3, 4]])
  |> should.equal([1, 2, 3, 4])
}

pub fn flat_map_test() {
  list.flat_map([1, 10, 20], fn(x) { [x, x + 1] })
  |> should.equal([1, 2, 10, 11, 20, 21])
}

pub fn fold_test() {
  [1, 2, 3]
  |> list.fold([], fn(acc, x) { [x, ..acc] })
  |> should.equal([3, 2, 1])
}

pub fn fold_right_test() {
  [1, 2, 3]
  |> list.fold_right(from: [], with: fn(acc, x) { [x, ..acc] })
  |> should.equal([1, 2, 3])
}

pub fn index_fold_test() {
  ["a", "b", "c"]
  |> list.index_fold([], fn(acc, i, ix) { [#(ix, i), ..acc] })
  |> should.equal([#(2, "c"), #(1, "b"), #(0, "a")])
}

pub fn fold_until_test() {
  [1, 2, 3, 4]
  |> list.fold_until(
    from: 0,
    with: fn(acc, n) {
      case n < 4 {
        True -> list.Continue(acc + n)
        False -> list.Stop(acc)
      }
    },
  )
  |> should.equal(6)
}

pub fn try_fold_test() {
  [1, 2, 3]
  |> list.try_fold(
    0,
    fn(i, acc) {
      case i < 4 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    },
  )
  |> should.equal(Ok(6))

  [1, 2, 3]
  |> list.try_fold(
    0,
    fn(i, acc) {
      case i < 3 {
        True -> Ok(acc + i)
        False -> Error(Nil)
      }
    },
  )
  |> should.equal(Error(Nil))
}

pub fn find_map_test() {
  let f = fn(x) {
    case x {
      2 -> Ok(4)
      _ -> Error(Nil)
    }
  }

  [1, 2, 3]
  |> list.find_map(with: f)
  |> should.equal(Ok(4))

  [1, 3, 2]
  |> list.find_map(with: f)
  |> should.equal(Ok(4))

  [1, 3]
  |> list.find_map(with: f)
  |> should.equal(Error(Nil))
}

pub fn find_test() {
  let is_two = fn(x) { x == 2 }

  [1, 2, 3]
  |> list.find(one_that: is_two)
  |> should.equal(Ok(2))

  [1, 3, 2]
  |> list.find(one_that: is_two)
  |> should.equal(Ok(2))

  [1, 3]
  |> list.find(one_that: is_two)
  |> should.equal(Error(Nil))
}

pub fn all_test() {
  list.all([1, 2, 3, 4, 5], fn(x) { x > 0 })
  |> should.equal(True)

  list.all([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> should.equal(False)

  list.all([], fn(_) { False })
  |> should.equal(True)
}

pub fn any_test() {
  list.any([1, 2, 3, 4, 5], fn(x) { x == 2 })
  |> should.equal(True)

  list.any([1, 2, 3, 4, 5], fn(x) { x < 0 })
  |> should.equal(False)

  list.any([], fn(_) { False })
  |> should.equal(False)
}

pub fn zip_test() {
  list.zip([], [1, 2, 3])
  |> should.equal([])

  list.zip([1, 2], [])
  |> should.equal([])

  list.zip([1, 2, 3], [4, 5, 6])
  |> should.equal([#(1, 4), #(2, 5), #(3, 6)])

  list.zip([5, 6], [1, 2, 3])
  |> should.equal([#(5, 1), #(6, 2)])

  list.zip([5, 6, 7], [1, 2])
  |> should.equal([#(5, 1), #(6, 2)])
}

pub fn strict_zip_test() {
  list.strict_zip([], [1, 2, 3])
  |> should.equal(Error(list.LengthMismatch))

  list.strict_zip([1, 2], [])
  |> should.equal(Error(list.LengthMismatch))

  list.strict_zip([1, 2, 3], [4, 5, 6])
  |> should.equal(Ok([#(1, 4), #(2, 5), #(3, 6)]))

  list.strict_zip([5, 6], [1, 2, 3])
  |> should.equal(Error(list.LengthMismatch))

  list.strict_zip([5, 6, 7], [1, 2])
  |> should.equal(Error(list.LengthMismatch))
}

pub fn unzip_test() {
  list.unzip([#(1, 2), #(3, 4)])
  |> should.equal(#([1, 3], [2, 4]))

  list.unzip([])
  |> should.equal(#([], []))
}

pub fn intersperse_test() {
  list.intersperse([1, 2, 3], 4)
  |> should.equal([1, 4, 2, 4, 3])

  list.intersperse([], 2)
  |> should.equal([])
}

pub fn at_test() {
  list.at([1, 2, 3], 2)
  |> should.equal(Ok(3))

  list.at([1, 2, 3], 5)
  |> should.equal(Error(Nil))

  list.at([], 0)
  |> should.equal(Error(Nil))

  list.at([1, 2, 3, 4, 5, 6], -1)
  |> should.equal(Ok(1))
}

pub fn unique_test() {
  list.unique([1, 1, 2, 3, 4, 4, 4, 5, 6])
  |> should.equal([1, 2, 3, 4, 5, 6])

  list.unique([7, 1, 45, 6, 2, 47, 2, 7, 5])
  |> should.equal([7, 1, 45, 6, 2, 47, 5])

  list.unique([3, 4, 5])
  |> should.equal([3, 4, 5])

  list.unique([])
  |> should.equal([])
}

pub fn sort_test() {
  [4, 3, 6, 5, 4]
  |> list.sort(int.compare)
  |> should.equal([3, 4, 4, 5, 6])

  [4, 3, 6, 5, 4, 1]
  |> list.sort(int.compare)
  |> should.equal([1, 3, 4, 4, 5, 6])

  [4.1, 3.1, 6.1, 5.1, 4.1]
  |> list.sort(float.compare)
  |> should.equal([3.1, 4.1, 4.1, 5.1, 6.1])

  []
  |> list.sort(int.compare)
  |> should.equal([])
}

pub fn index_map_test() {
  list.index_map([3, 4, 5], fn(i, x) { #(i, x) })
  |> should.equal([#(0, 3), #(1, 4), #(2, 5)])

  let f = fn(i, x) { #(x, i) }
  list.index_map(["a", "b"], f)
  |> should.equal([#("a", 0), #("b", 1)])

  let f = fn(i, x) { #(x, i) }
  list.index_map(["a", "b", "c"], f)
  |> should.equal([#("a", 0), #("b", 1), #("c", 2)])
}

pub fn range_test() {
  list.range(0, 0)
  |> should.equal([])

  list.range(1, 1)
  |> should.equal([])

  list.range(-1, -1)
  |> should.equal([])

  list.range(0, 1)
  |> should.equal([0])

  list.range(0, 5)
  |> should.equal([0, 1, 2, 3, 4])

  list.range(1, -5)
  |> should.equal([1, 0, -1, -2, -3, -4])
}

pub fn repeat_test() {
  list.repeat(1, -10)
  |> should.equal([])

  list.repeat(1, 0)
  |> should.equal([])

  list.repeat(2, 3)
  |> should.equal([2, 2, 2])

  list.repeat("x", 5)
  |> should.equal(["x", "x", "x", "x", "x"])
}

pub fn split_test() {
  []
  |> list.split(0)
  |> should.equal(#([], []))

  [0, 1, 2, 3, 4]
  |> list.split(0)
  |> should.equal(#([], [0, 1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(-2)
  |> should.equal(#([], [0, 1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(1)
  |> should.equal(#([0], [1, 2, 3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(3)
  |> should.equal(#([0, 1, 2], [3, 4]))

  [0, 1, 2, 3, 4]
  |> list.split(9)
  |> should.equal(#([0, 1, 2, 3, 4], []))
}

pub fn split_while_test() {
  []
  |> list.split_while(fn(x) { x <= 5 })
  |> should.equal(#([], []))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= 5 })
  |> should.equal(#([1, 2, 3, 4, 5], []))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x == 2 })
  |> should.equal(#([], [1, 2, 3, 4, 5]))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= 3 })
  |> should.equal(#([1, 2, 3], [4, 5]))

  [1, 2, 3, 4, 5]
  |> list.split_while(fn(x) { x <= -3 })
  |> should.equal(#([], [1, 2, 3, 4, 5]))
}

pub fn key_find_test() {
  let proplist = [#(0, "1"), #(1, "2")]

  proplist
  |> list.key_find(0)
  |> should.equal(Ok("1"))

  proplist
  |> list.key_find(1)
  |> should.equal(Ok("2"))

  proplist
  |> list.key_find(2)
  |> should.equal(Error(Nil))
}

pub fn pop_test() {
  list.pop([1, 2, 3], fn(x) { x > 2 })
  |> should.equal(Ok(#(3, [1, 2])))

  list.pop([1, 2, 3], fn(x) { x > 4 })
  |> should.equal(Error(Nil))

  list.pop([], fn(_x) { True })
  |> should.equal(Error(Nil))
}

pub fn pop_map_test() {
  let get = fn(x) {
    case x > 0 {
      True -> Ok(x * 2)
      False -> Error(Nil)
    }
  }
  list.pop_map([0, 2, 3], get)
  |> should.equal(Ok(#(4, [0, 3])))

  list.pop_map([0, -1], get)
  |> should.equal(Error(Nil))

  list.pop_map([], get)
  |> should.equal(Error(Nil))
}

pub fn key_pop_test() {
  list.key_pop([#("a", 0), #("b", 1)], "a")
  |> should.equal(Ok(#(0, [#("b", 1)])))

  list.key_pop([#("a", 0), #("b", 1)], "b")
  |> should.equal(Ok(#(1, [#("a", 0)])))

  list.key_pop([#("a", 0), #("b", 1)], "c")
  |> should.equal(Error(Nil))
}

pub fn key_set_test() {
  [#(5, 0), #(4, 1)]
  |> list.key_set(4, 100)
  |> should.equal([#(5, 0), #(4, 100)])

  [#(5, 0), #(4, 1)]
  |> list.key_set(1, 100)
  |> should.equal([#(5, 0), #(4, 1), #(1, 100)])
}

pub fn partition_test() {
  [1, 2, 3, 4, 5, 6, 7]
  |> list.partition(int.is_odd)
  |> should.equal(#([1, 3, 5, 7], [2, 4, 6]))
}

pub fn permutations_test() {
  [1, 2]
  |> list.permutations
  |> should.equal([[1, 2], [2, 1]])

  let expected = [
    [1, 2, 3],
    [1, 3, 2],
    [2, 1, 3],
    [2, 3, 1],
    [3, 1, 2],
    [3, 2, 1],
  ]

  [1, 2, 3]
  |> list.permutations
  |> should.equal(expected)

  ["a", "b"]
  |> list.permutations
  |> should.equal([["a", "b"], ["b", "a"]])
}

pub fn window_test() {
  [1, 2, 3]
  |> list.window(by: 2)
  |> should.equal([[1, 2], [2, 3]])

  [1, 2, 3]
  |> list.window(3)
  |> should.equal([[1, 2, 3]])

  [1, 2, 3]
  |> list.window(4)
  |> should.equal([])

  [1, 2, 3, 4, 5]
  |> list.window(3)
  |> should.equal([[1, 2, 3], [2, 3, 4], [3, 4, 5]])
}

pub fn window_by_2_test() {
  [1, 2, 3, 4]
  |> list.window_by_2
  |> should.equal([#(1, 2), #(2, 3), #(3, 4)])

  [1]
  |> list.window_by_2
  |> should.equal([])
}

pub fn drop_while_test() {
  [1, 2, 3, 4]
  |> list.drop_while(fn(x) { x < 3 })
  |> should.equal([3, 4])
}

pub fn take_while_test() {
  [1, 2, 3, 2, 4]
  |> list.take_while(fn(x) { x < 3 })
  |> should.equal([1, 2])
}

pub fn chunk_test() {
  [1, 2, 3]
  |> list.chunk(by: fn(n) { n % 2 })
  |> should.equal([[1], [2], [3]])

  [1, 2, 2, 3, 4, 4, 6, 7, 7]
  |> list.chunk(by: fn(n) { n % 2 })
  |> should.equal([[1], [2, 2], [3], [4, 4, 6], [7, 7]])
}

pub fn sized_chunk_test() {
  [1, 2, 3, 4, 5, 6]
  |> list.sized_chunk(into: 2)
  |> should.equal([[1, 2], [3, 4], [5, 6]])

  [1, 2, 3, 4, 5, 6, 7, 8]
  |> list.sized_chunk(into: 3)
  |> should.equal([[1, 2, 3], [4, 5, 6], [7, 8]])
}

pub fn reduce_test() {
  []
  |> list.reduce(with: fn(x, y) { x + y })
  |> should.equal(Error(Nil))

  [1, 2, 3, 4, 5]
  |> list.reduce(with: fn(x, y) { x + y })
  |> should.equal(Ok(15))
}

pub fn scan_test() {
  []
  |> list.scan(from: 0, with: fn(acc, i) { i + acc })
  |> should.equal([])

  [1, 2, 3, 4]
  |> list.scan(from: 0, with: fn(acc, i) { 2 * i + acc })
  |> should.equal([2, 6, 12, 20])

  [1, 2, 3, 4]
  |> list.scan(
    from: [],
    with: fn(acc, i) {
      case int.is_even(i) {
        True -> ["Even", ..acc]
        False -> ["Odd", ..acc]
      }
    },
  )
  |> should.equal([
    ["Odd"],
    ["Even", "Odd"],
    ["Odd", "Even", "Odd"],
    ["Even", "Odd", "Even", "Odd"],
  ])
}

pub fn last_test() {
  list.last([])
  |> should.equal(Error(Nil))

  list.last([1, 2, 3, 4, 5])
  |> should.equal(Ok(5))
}

pub fn combinations_test() {
  list.combinations([1, 2, 3], by: 0)
  |> should.equal([[]])

  list.combinations([1, 2, 3], by: 1)
  |> should.equal([[1], [2], [3]])

  list.combinations([1, 2, 3], by: 2)
  |> should.equal([[1, 2], [1, 3], [2, 3]])

  list.combinations([1, 2, 3], by: 3)
  |> should.equal([[1, 2, 3]])

  list.combinations([1, 2, 3], by: 4)
  |> should.equal([])

  list.combinations([1, 2, 3, 4], 3)
  |> should.equal([[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]])
}

pub fn combination_pairs_test() {
  list.combination_pairs([1])
  |> should.equal([])

  list.combination_pairs([1, 2])
  |> should.equal([#(1, 2)])

  list.combination_pairs([1, 2, 3])
  |> should.equal([#(1, 2), #(1, 3), #(2, 3)])

  list.combination_pairs([1, 2, 3, 4])
  |> should.equal([#(1, 2), #(1, 3), #(1, 4), #(2, 3), #(2, 4), #(3, 4)])
}

pub fn interleave_test() {
  list.interleave([[1, 2], [101, 102]])
  |> should.equal([1, 101, 2, 102])

  list.interleave([[1, 2], [101, 102], [201, 202]])
  |> should.equal([1, 101, 201, 2, 102, 202])

  // Left over elements are added at the end
  list.interleave([[1, 2, 3], [101, 102]])
  |> should.equal([1, 101, 2, 102, 3])

  list.interleave([[1, 2], [101, 102, 103]])
  |> should.equal([1, 101, 2, 102, 103])
}

pub fn transpose_test() {
  list.transpose([[1, 2, 3], [101, 102, 103]])
  |> should.equal([[1, 101], [2, 102], [3, 103]])

  list.transpose([[1, 2, 3], [101, 102, 103], [201, 202, 203]])
  |> should.equal([[1, 101, 201], [2, 102, 202], [3, 103, 203]])

  // Left over elements are still returned
  list.transpose([[1, 2], [101, 102, 103]])
  |> should.equal([[1, 101], [2, 102], [103]])

  list.transpose([[1, 2, 3], [101, 102], [201, 202, 203]])
  |> should.equal([[1, 101, 201], [2, 102, 202], [3, 203]])
}
