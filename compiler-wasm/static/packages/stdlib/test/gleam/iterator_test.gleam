import gleam/should
import gleam/iterator.{Done, Next}
import gleam/list
import gleam/map

// a |> from_list |> to_list == a
pub fn to_from_list_test() {
  let test = fn(subject) {
    subject
    |> iterator.from_list
    |> iterator.to_list
    |> should.equal(subject)
  }

  test([])
  test([1])
  test([1, 2])
  test([1, 2, 4, 8])
}

pub fn step_test() {
  let test = fn(subject) {
    let step =
      subject
      |> iterator.from_list
      |> iterator.step

    case subject {
      [] ->
        step
        |> should.equal(Done)

      [h, ..t] -> {
        assert Next(h2, t2) = step
        h
        |> should.equal(h2)
        t2
        |> iterator.to_list
        |> should.equal(t)
      }
    }
  }

  test([])
  test([1])
  test([1, 2])
  test([1, 2, 3])
}

// a |> from_list |> take(n) == a |> list.take(_, n)
pub fn take_test() {
  let test = fn(n, subject) {
    subject
    |> iterator.from_list
    |> iterator.take(n)
    |> iterator.to_list
    |> should.equal(list.take(subject, n))
  }

  test(0, [])
  test(1, [])
  test(-1, [])
  test(0, [0])
  test(1, [0])
  test(-1, [0])
  test(0, [0, 1, 2, 3, 4])
  test(1, [0, 1, 2, 3, 4])
  test(2, [0, 1, 2, 3, 4])
  test(22, [0, 1, 2, 3, 4])
}

// a |> from_list |> fold(a, f) == a |> list.fold(_, a, f)
pub fn fold_test() {
  let test = fn(subject, acc, f) {
    subject
    |> iterator.from_list
    |> iterator.fold(acc, f)
    |> should.equal(list.fold(subject, acc, f))
  }

  let f = fn(acc, e) { [e, ..acc] }
  test([], [], f)
  test([1], [], f)
  test([1, 2, 3], [], f)
  test([1, 2, 3, 4, 5, 6, 7, 8], [], f)
}

// a |> from_list |> map(f) |> to_list == a |> list.map(_, f)
pub fn map_test() {
  let test = fn(subject, f) {
    subject
    |> iterator.from_list
    |> iterator.map(f)
    |> iterator.to_list
    |> should.equal(list.map(subject, f))
  }

  let f = fn(e) { e * 2 }
  test([], f)
  test([1], f)
  test([1, 2, 3], f)
  test([1, 2, 3, 4, 5, 6, 7, 8], f)
}

// a |> from_list |> flat_map(f) |> to_list ==
//   a |> list.map(f) |> list.map(to_list) |> list.flatten
pub fn flat_map_test() {
  let test = fn(subject, f) {
    subject
    |> iterator.from_list
    |> iterator.flat_map(f)
    |> iterator.to_list
    |> should.equal(
      subject
      |> list.map(f)
      |> list.map(iterator.to_list)
      |> list.flatten,
    )
  }

  let f = fn(i) { iterator.range(i, i + 2) }

  test([], f)
  test([1], f)
  test([1, 2], f)
}

// a |> from_list |> append(from_list(b)) |> to_list == list.flatten([a, b])
pub fn append_test() {
  let test = fn(left, right) {
    left
    |> iterator.from_list
    |> iterator.append(iterator.from_list(right))
    |> iterator.to_list
    |> should.equal(list.flatten([left, right]))
  }

  test([], [])
  test([1], [2])
  test([1, 2], [3, 4])
}

// a |> list.map(from_list) |> flatten |> to_list == list.flatten(a)
pub fn flatten_test() {
  let test = fn(lists) {
    lists
    |> list.map(iterator.from_list)
    |> iterator.from_list
    |> iterator.flatten
    |> iterator.to_list
    |> should.equal(list.flatten(lists))
  }

  test([[], []])
  test([[1], [2]])
  test([[1, 2], [3, 4]])
}

// a |> from_list |> filter(f) |> to_list == a |> list.filter(_, f)
pub fn filter_test() {
  let test = fn(subject, f) {
    subject
    |> iterator.from_list
    |> iterator.filter(f)
    |> iterator.to_list
    |> should.equal(list.filter(subject, f))
  }

  let even = fn(x) { x % 2 == 0 }
  test([], even)
  test([1], even)
  test([1, 2], even)
  test([1, 2, 3], even)
  test([1, 2, 3, 4], even)
  test([1, 2, 3, 4, 5], even)
  test([1, 2, 3, 4, 5, 6], even)
}

pub fn repeat_test() {
  1
  |> iterator.repeat
  |> iterator.take(5)
  |> iterator.to_list
  |> should.equal([1, 1, 1, 1, 1])
}

pub fn cycle_test() {
  [1, 2, 3]
  |> iterator.from_list
  |> iterator.cycle
  |> iterator.take(9)
  |> iterator.to_list
  |> should.equal([1, 2, 3, 1, 2, 3, 1, 2, 3])
}

pub fn unfold_test() {
  iterator.unfold(2, fn(acc) { iterator.Next(acc, acc * 2) })
  |> iterator.take(5)
  |> iterator.to_list
  |> should.equal([2, 4, 8, 16, 32])

  iterator.unfold(2, fn(_) { iterator.Done })
  |> iterator.take(5)
  |> iterator.to_list
  |> should.equal([])

  fn(n) {
    case n {
      0 -> iterator.Done
      n -> iterator.Next(element: n, accumulator: n - 1)
    }
  }
  |> iterator.unfold(from: 5)
  |> iterator.to_list
  |> should.equal([5, 4, 3, 2, 1])
}

pub fn range_test() {
  let test = fn(a, b, expected) {
    iterator.range(a, b)
    |> iterator.to_list
    |> should.equal(expected)
  }

  test(0, 0, [])
  test(1, 1, [])
  test(-1, -1, [])
  test(0, 1, [0])
  test(0, 5, [0, 1, 2, 3, 4])
  test(1, -5, [1, 0, -1, -2, -3, -4])
}

pub fn drop_test() {
  iterator.range(0, 10)
  |> iterator.drop(5)
  |> iterator.to_list
  |> should.equal([5, 6, 7, 8, 9])
}

type Cat {
  Cat(id: Int)
}

pub fn find_test() {
  iterator.range(0, 10)
  |> iterator.find(fn(e) { e == 5 })
  |> should.equal(Ok(5))

  iterator.range(0, 10)
  |> iterator.find(fn(e) { e > 10 })
  |> should.equal(Error(Nil))

  iterator.empty()
  |> iterator.find(fn(_x) { True })
  |> should.equal(Error(Nil))

  iterator.unfold(
    Cat(id: 1),
    fn(cat: Cat) { iterator.Next(cat, Cat(id: cat.id + 1)) },
  )
  |> iterator.find(fn(cat: Cat) { cat.id == 10 })
  |> should.equal(Ok(Cat(id: 10)))
}

pub fn index_test() {
  iterator.from_list(["a", "b", "c"])
  |> iterator.index
  |> iterator.to_list
  |> should.equal([#(0, "a"), #(1, "b"), #(2, "c")])
}

pub fn iterate_test() {
  fn(x) { x * 3 }
  |> iterator.iterate(from: 1)
  |> iterator.take(5)
  |> iterator.to_list
  |> should.equal([1, 3, 9, 27, 81])
}

pub fn take_while_test() {
  iterator.from_list([1, 2, 3, 2, 4])
  |> iterator.take_while(satisfying: fn(x) { x < 3 })
  |> iterator.to_list
  |> should.equal([1, 2])
}

pub fn drop_while_test() {
  iterator.from_list([1, 2, 3, 4, 2, 5])
  |> iterator.drop_while(satisfying: fn(x) { x < 4 })
  |> iterator.to_list
  |> should.equal([4, 2, 5])
}

pub fn scan_test() {
  iterator.from_list([1, 2, 3, 4, 5])
  |> iterator.scan(from: 0, with: fn(el, acc) { acc + el })
  |> iterator.to_list
  |> should.equal([1, 3, 6, 10, 15])
}

pub fn zip_test() {
  iterator.from_list(["a", "b", "c"])
  |> iterator.zip(iterator.range(20, 30))
  |> iterator.to_list
  |> should.equal([#("a", 20), #("b", 21), #("c", 22)])
}

pub fn chunk_test() {
  iterator.from_list([1, 2, 2, 3, 4, 4, 6, 7, 7])
  |> iterator.chunk(by: fn(n) { n % 2 })
  |> iterator.to_list
  |> should.equal([[1], [2, 2], [3], [4, 4, 6], [7, 7]])
}

pub fn sized_chunk_test() {
  iterator.from_list([1, 2, 3, 4, 5, 6])
  |> iterator.sized_chunk(into: 2)
  |> iterator.to_list
  |> should.equal([[1, 2], [3, 4], [5, 6]])

  iterator.from_list([1, 2, 3, 4, 5, 6, 7, 8])
  |> iterator.sized_chunk(into: 3)
  |> iterator.to_list
  |> should.equal([[1, 2, 3], [4, 5, 6], [7, 8]])
}

pub fn intersperse_test() {
  iterator.empty()
  |> iterator.intersperse(with: 0)
  |> iterator.to_list
  |> should.equal([])

  iterator.from_list([1])
  |> iterator.intersperse(with: 0)
  |> iterator.to_list
  |> should.equal([1])

  iterator.from_list([1, 2, 3, 4, 5])
  |> iterator.intersperse(with: 0)
  |> iterator.to_list
  |> should.equal([1, 0, 2, 0, 3, 0, 4, 0, 5])
}

pub fn any_test() {
  iterator.empty()
  |> iterator.any(satisfying: fn(n) { n % 2 == 0 })
  |> should.be_false

  iterator.from_list([1, 2, 5, 7, 9])
  |> iterator.any(satisfying: fn(n) { n % 2 == 0 })
  |> should.be_true

  iterator.from_list([1, 3, 5, 7, 9])
  |> iterator.any(satisfying: fn(n) { n % 2 == 0 })
  |> should.be_false
}

pub fn all_test() {
  iterator.empty()
  |> iterator.all(satisfying: fn(n) { n % 2 == 0 })
  |> should.be_true

  iterator.from_list([2, 4, 6, 8])
  |> iterator.all(satisfying: fn(n) { n % 2 == 0 })
  |> should.be_true

  iterator.from_list([2, 4, 5, 8])
  |> iterator.all(satisfying: fn(n) { n % 2 == 0 })
  |> should.be_false
}

pub fn group_test() {
  iterator.from_list([1, 2, 3, 4, 5, 6])
  |> iterator.group(by: fn(n) { n % 3 })
  |> should.equal(map.from_list([#(0, [3, 6]), #(1, [1, 4]), #(2, [2, 5])]))
}

pub fn reduce_test() {
  iterator.empty()
  |> iterator.reduce(with: fn(x, y) { x + y })
  |> should.equal(Error(Nil))

  iterator.from_list([1, 2, 3, 4, 5])
  |> iterator.reduce(with: fn(x, y) { x + y })
  |> should.equal(Ok(15))
}

pub fn last_test() {
  iterator.empty()
  |> iterator.last
  |> should.equal(Error(Nil))

  iterator.range(1, 10)
  |> iterator.last
  |> should.equal(Ok(9))
}

pub fn empty_test() {
  iterator.empty()
  |> iterator.to_list
  |> should.equal([])
}

pub fn once_test() {
  iterator.once(fn() { 1 })
  |> iterator.to_list
  |> should.equal([1])
}

pub fn single_test() {
  iterator.single(1)
  |> iterator.to_list
  |> should.equal([1])
}

pub fn interleave_test() {
  iterator.from_list([1, 2, 3, 4])
  |> iterator.interleave(with: iterator.from_list([11, 12, 13, 14]))
  |> iterator.to_list
  |> should.equal([1, 11, 2, 12, 3, 13, 4, 14])

  iterator.from_list([1, 2, 3, 4])
  |> iterator.interleave(with: iterator.from_list([100]))
  |> iterator.to_list
  |> should.equal([1, 100, 2, 3, 4])
}

// a |> from_list |> fold_until(acc, f) == a |> list.fold_until(acc, f)
pub fn fold_until_test() {
  let test = fn(subject, acc, f) {
    subject
    |> iterator.from_list()
    |> iterator.fold_until(acc, f)
    |> should.equal(list.fold_until(subject, acc, f))
  }

  let f = fn(acc, e) {
    case e {
      _ if e < 6 -> list.Continue([e, ..acc])
      _ -> list.Stop(acc)
    }
  }
  test([], [], f)
  test([1], [], f)
  test([1, 2, 3], [], f)
  test([1, 2, 3, 4, 5, 6, 7, 8], [], f)

  [1, 2, 3, 4, 5, 6, 7, 8]
  |> iterator.from_list()
  |> iterator.fold_until([], f)
  |> should.equal([5, 4, 3, 2, 1])
}

// a |> from_list |> try_fold(acc, f) == a |> list.try_fold(acc, f)
pub fn try_fold_test() {
  let test = fn(subject, acc, fun) {
    subject
    |> iterator.from_list()
    |> iterator.try_fold(acc, fun)
    |> should.equal(list.try_fold(subject, acc, fun))
  }

  let f = fn(e, acc) {
    case e % 2 {
      0 -> Ok(e + acc)
      _ -> Error("tried to add an odd number")
    }
  }
  test([], 0, f)
  test([2, 4, 6], 0, f)
  test([1, 2, 3], 0, f)
  test([1, 2, 3, 4, 5, 6, 7, 8], 0, f)

  [0, 2, 4, 6]
  |> iterator.from_list()
  |> iterator.try_fold(0, f)
  |> should.equal(Ok(12))

  [1, 2, 3, 4]
  |> iterator.from_list()
  |> iterator.try_fold(0, f)
  |> should.equal(Error("tried to add an odd number"))
}

pub fn first_test() {
  iterator.from_list([1, 2, 3])
  |> iterator.first
  |> should.equal(Ok(1))

  iterator.empty()
  |> iterator.first
  |> should.equal(Error(Nil))
}

pub fn at_test() {
  iterator.from_list([1, 2, 3, 4])
  |> iterator.at(2)
  |> should.equal(Ok(3))

  iterator.from_list([1, 2, 3, 4])
  |> iterator.at(4)
  |> should.equal(Error(Nil))

  iterator.empty()
  |> iterator.at(0)
  |> should.equal(Error(Nil))
}
