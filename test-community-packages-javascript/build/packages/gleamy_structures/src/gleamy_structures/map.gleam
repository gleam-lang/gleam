import gleam/order.{Order}
import gleam/list
import gleamy_structures/tree/red_black_tree_kv as tree

type Map(k, v) =
  tree.Tree(k, v)

pub fn new(compare: fn(k, k) -> Order) -> Map(k, v) {
  tree.new(compare)
}

pub fn insert(into map: Map(k, v), key key: k, value value: v) -> Map(k, v) {
  tree.insert(map, key, value)
}

pub fn find(in map: Map(k, v), key key: k) -> Result(#(k, v), Nil) {
  tree.find(map, key)
}

pub fn has_key(in map: Map(k, v), key key: k) -> Bool {
  case tree.find(map, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn delete(from map: Map(k, v), this key: k) -> Map(k, v) {
  tree.delete(map, key)
}

pub fn count(map: Map(k, v)) -> Int {
  tree.fold(map, 0, fn(a, _, _) { a + 1 })
}

pub fn fold(
  over map: Map(k, v),
  from initial: a,
  with reducer: fn(a, k, v) -> a,
) -> a {
  tree.fold(map, initial, reducer)
}

pub fn filter(in map: Map(k, v), for property: fn(k, v) -> Bool) -> Map(k, v) {
  tree.fold(
    map,
    map,
    fn(map, k, v) {
      case property(k, v) {
        True -> map
        False -> tree.delete(map, k)
      }
    },
  )
}

pub fn merge(this first: Map(k, v), and second: Map(k, v)) -> Map(k, v) {
  tree.fold(first, second, fn(a, k, v) { tree.insert(a, k, v) })
}

// return the map keeping only keys in the list
pub fn take(from map: Map(k, v), keeping desired: List(k)) -> Map(k, v) {
  case desired {
    [x, ..xs] ->
      case tree.find(map, x) {
        Ok(x) -> tree.insert(take(map, xs), x.0, x.1)
        Error(_) -> take(map, xs)
      }
    [] -> tree.clear(map)
  }
}

pub fn from_list(
  members: List(#(k, v)),
  compare: fn(k, k) -> Order,
) -> Map(k, v) {
  list.fold(
    members,
    tree.new(compare),
    fn(tree, i) { tree.insert(tree, i.0, i.1) },
  )
}

pub fn to_list(map: Map(k, v)) -> List(#(k, v)) {
  tree.foldr(map, [], fn(a, k, v) { [#(k, v), ..a] })
}
