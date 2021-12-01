import gleam/should
import gleam/set
import gleam/list
import gleam/int

pub fn size_test() {
  set.new()
  |> set.size
  |> should.equal(0)

  set.new()
  |> set.insert(1)
  |> set.insert(2)
  |> set.size
  |> should.equal(2)

  set.new()
  |> set.insert(1)
  |> set.insert(1)
  |> set.insert(2)
  |> set.size
  |> should.equal(2)
}

pub fn contains_test() {
  set.new()
  |> set.insert(1)
  |> set.contains(this: 1)
  |> should.be_true

  set.new()
  |> set.contains(this: 1)
  |> should.be_false
}

pub fn delete_test() {
  set.new()
  |> set.insert(1)
  |> set.delete(1)
  |> set.contains(1)
  |> should.be_false
}

pub fn to_list_test() {
  set.new()
  |> set.insert(2)
  |> set.insert(3)
  |> set.insert(4)
  |> set.to_list
  |> list.sort(by: int.compare)
  |> should.equal([2, 3, 4])
}

pub fn from_list_test() {
  [1, 1, 2, 4, 3, 2]
  |> set.from_list
  |> set.to_list
  |> list.sort(by: int.compare)
  |> should.equal([1, 2, 3, 4])
}

pub fn fold_test() {
  [1, 3, 9]
  |> set.from_list
  |> set.fold(from: 0, with: fn(m, a) { m + a })
}

pub fn filter_test() {
  [1, 4, 6, 3, 675, 44, 67]
  |> set.from_list()
  |> set.filter(for: int.is_even)
  |> set.to_list
  |> should.equal([4, 6, 44])
}

pub fn take_test() {
  [1, 2, 3]
  |> set.from_list
  |> set.take([1, 3, 5])
  |> should.equal(set.from_list([1, 3]))
}

pub fn union_test() {
  set.union(set.from_list([1, 2]), set.from_list([2, 3]))
  |> set.to_list
  |> list.sort(int.compare)
  |> should.equal([1, 2, 3])
}

pub fn intersection_test() {
  set.intersection(set.from_list([1, 2]), set.from_list([2, 3]))
  |> set.to_list
  |> should.equal([2])
}
