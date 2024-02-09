import gleam/order.{Order}
import gleam/list
import gleam/io
import gleam/string
import gleamy_structures/tree/red_black_tree as tree

type Set(a) =
  tree.Tree(a)

pub fn contains(in set: Set(a), this member: a) -> Bool {
  case tree.find(set, member) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn delete(from set: Set(a), this member: a) -> Set(a) {
  tree.delete(set, member)
}

pub fn filter(in set: Set(a), for property: fn(a) -> Bool) -> Set(a) {
  tree.fold(
    set,
    set,
    fn(set, i) {
      case property(i) {
        True -> set
        False -> tree.delete(set, i)
      }
    },
  )
}

pub fn fold(over set: Set(a), from initial: b, with reducer: fn(b, a) -> b) -> b {
  tree.fold(set, initial, reducer)
}

pub fn from_list(members: List(a), compare: fn(a, a) -> Order) -> Set(a) {
  list.fold(members, tree.new(compare), tree.insert)
}

pub fn insert(into set: Set(a), this member: a) -> Set(a) {
  tree.insert(set, member)
}

pub fn intersection(of first: Set(a), and second: Set(a)) -> Set(a) {
  tree.fold(
    second,
    tree.clear(first),
    fn(a, i) {
      case tree.find(first, i) {
        Ok(_) -> tree.insert(a, i)
        Error(_) -> a
      }
    },
  )
}

pub fn new(compare: fn(a, a) -> Order) -> Set(a) {
  tree.new(compare)
}

pub fn count(set: Set(a)) -> Int {
  tree.fold(set, 0, fn(a, _) { a + 1 })
}

pub fn take(from set: Set(a), keeping desired: List(a)) -> Set(a) {
  case desired {
    [x, ..xs] ->
      case tree.find(set, x) {
        Ok(x) -> tree.insert(take(set, xs), x)
        Error(_) -> take(set, xs)
      }
    [] -> tree.clear(set)
  }
}

pub fn to_list(set: Set(a)) -> List(a) {
  tree.foldr(
    set,
    [],
    fn(a, i) {
      io.println(string.inspect(i))
      [i, ..a]
    },
  )
}

pub fn union(of first: Set(a), and second: Set(a)) -> Set(a) {
  tree.fold(first, second, fn(a, i) { tree.insert(a, i) })
}
