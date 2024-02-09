import gleam/list
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import gleam/result

/// A `Trie(k, v)` is a data structure that allows to store values of type `v` indexed by lists
/// of values of type `k`.
/// 
pub opaque type Trie(k, v) {
  /// The trie constructor, its implementation is based on the one described by Okasaki in
  /// Purely Functional Data Structures.
  /// 
  Trie(entry: Option(v), children_map: Map(k, Trie(k, v)))
}

/// Deletes from a trie the value associated with a given path.
/// 
/// ## Examples
/// 
/// ```gleam
/// > [#([1, 2], "a"), #([1], "b")]
/// > |> from_list
/// > |> delete(at: [1, 2])
/// > |> to_list
/// [#([1], "b")]
/// ```
/// 
/// ```gleam
/// > new()
/// > |> delete(at: [1, 2])
/// > |> to_list
/// []
/// ```
/// 
pub fn delete(from trie: Trie(k, v), at path: List(k)) -> Trie(k, v) {
  do_delete(from: trie, at: path)
  |> option.unwrap(new())
}

/// Exactly same behaviour as delete but returns `None` if the tree is empty as a
/// result of the deletion.
///
fn do_delete(from trie: Trie(k, v), at path: List(k)) -> Option(Trie(k, v)) {
  case path, trie {
    [], Trie(_, children_map) ->
      case map.size(children_map) {
        0 -> None
        _ -> Some(Trie(None, children_map))
      }
    [first, ..rest], Trie(entry, children_map) -> {
      let new_children = case map.get(children_map, first) {
        Error(_) -> children_map
        Ok(child) ->
          case do_delete(from: child, at: rest) {
            None -> map.delete(children_map, first)
            Some(trie) -> map.insert(children_map, first, trie)
          }
      }
      case entry, map.size(new_children) {
        None, 0 -> None
        _, _ -> Some(Trie(entry, new_children))
      }
    }
  }
}

/// Combines all the trie's values into a single one by calling a given function on each one.
/// 
/// The function takes as input the accumulator, the path of a value and the corresponding value.
/// 
/// ## Examples
/// 
/// ```gleam
/// > [#([1, 2], 10), #([1], 1)]
/// > |> from_list
/// > |> fold(from: 0, with: fn(sum, _, value) { sum + value })
/// 11
/// ```
/// 
pub fn fold(
  over trie: Trie(k, a),
  from initial: b,
  with fun: fn(b, List(k), a) -> b,
) -> b {
  map.fold(
    over: trie.children_map,
    from: trie.entry
    |> option.map(fun(initial, [], _))
    |> option.unwrap(initial),
    with: fn(acc, first, trie) {
      fold(
        over: trie,
        from: acc,
        with: fn(acc, rest, value) { fun(acc, [first, ..rest], value) },
      )
    },
  )
}

/// Creates a new trie from a list of path-value pairs.
/// 
/// ## Examples
/// 
/// ```gleam
/// > [#([1, 2], "a"), #([1], "b")]
/// > |> from_list
/// > |> to_list
/// [#([1, 2], "a"), #([1], "b")]
/// ```
///
pub fn from_list(list: List(#(List(k), v))) -> Trie(k, v) {
  list.fold(
    over: list,
    from: new(),
    with: fn(trie, pair) { insert(trie, pair.0, pair.1) },
  )
}

/// Fetches a value from a trie for a given path.
/// If a value is present at the given path it returns it wrapped in an `Ok`,
/// otherwise it returns `Error(Nil)`.
/// 
/// ## Examples
/// 
/// ```gleam
/// > new()
/// > |> get(at: [1, 2])
/// Result(Nil)
/// ```
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> get(at: [1, 2])
/// Ok("a")
/// ```
/// 
pub fn get(from: Trie(k, v), at path: List(k)) -> Result(v, Nil) {
  case path, from {
    [], Trie(None, _) -> Error(Nil)
    [], Trie(Some(value), _) -> Ok(value)
    [first, ..rest], Trie(_, children_map) ->
      children_map
      |> map.get(first)
      |> result.then(get(_, rest))
  }
}

/// Determines wether a trie contains a value associated with the given path.
/// 
/// ## Examples
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> has_path([1, 2])
/// True
/// ```
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> has_path([1])
/// False
/// ```
/// 
pub fn has_path(trie: Trie(k, v), path: List(k)) -> Bool {
  case get(trie, path) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Inserts a value in a trie at a given path. If there already is a value
/// at the given path it is replaced by the new one.
/// 
/// ## Examples
/// 
/// ```gleam
/// > new()
/// > |> insert(at: [1, 2], value: "a")
/// > |> insert(at: [1], value: "b")
/// > |> to_list
/// [#([1, 2], "a"), #([1], "b")]
/// ```
/// 
/// ```gleam
/// > new()
/// > |> insert(at: [1, 2], value: "a")
/// > |> insert(at: [1, 2], value: "b")
/// > |> to_list
/// [#([1, 2], "b")]
/// ```
/// 
pub fn insert(
  into trie: Trie(k, v),
  at path: List(k),
  value value: v,
) -> Trie(k, v) {
  case path, trie {
    [], Trie(_, children_map) -> Trie(Some(value), children_map)
    [first, ..rest], Trie(entry, children_map) -> {
      map.get(children_map, first)
      |> result.unwrap(new())
      |> insert(rest, value)
      |> map.insert(children_map, first, _)
      |> Trie(entry, _)
    }
  }
}

/// Determines wether or not the trie is empty.
/// 
/// ## Examples
/// 
/// ```gleam
/// > new()
/// > |> is_empty
/// True
/// ```
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> is_empty
/// False
/// ```
/// 
pub fn is_empty(trie: Trie(k, v)) -> Bool {
  size(trie) == 0
}

/// Updates all the values in a given trie by calling a function on each value.
/// 
/// ## Examples
/// 
/// ```gleam
/// > [#([1, 2], "a"), #([1], "b")]
/// > |> from_list
/// > |> map(fn(s) { s <> "!" })
/// > |> to_list
/// [#([1, 2], "a!"), #([1], "b!")]
/// ```
/// 
pub fn map(over trie: Trie(k, v), with fun: fn(v) -> a) -> Trie(k, a) {
  Trie(
    option.map(trie.entry, fun),
    map.map_values(trie.children_map, fn(_, t) { map(t, fun) }),
  )
}

/// Creates a new empty trie.
/// 
/// ## Examples
/// 
/// ```gleam
/// > new()
/// > |> to_list
/// []
/// ```
/// 
pub fn new() -> Trie(k, v) {
  Trie(None, map.new())
}

/// Gets a list of all the valid paths in the trie. That is all the paths associated with a value.
/// 
/// Tries are not ordered so the paths are not returned in any specific order.
/// Do not write code that relies on the order paths are returned by this function
/// as it may change in later versions of the library.
/// 
/// ## Examples
/// 
/// ```gleam
/// > [#([1, 2], "a"), #([1], "b")]
/// > |> from_list
/// > |> paths
/// [[1, 2], [1]]
/// ```
/// 
/// ```gleam
/// > new()
/// > |> paths
/// []
/// ```
pub fn paths(trie: Trie(k, v)) -> List(List(k)) {
  fold(over: trie, from: [], with: fn(rest, path, _) { [path, ..rest] })
}

/// Creates a new trie with a single value associated to the given path.
/// 
/// ## Examples
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> to_list
/// [#([1, 2], "a")]
/// ```
/// 
pub fn singleton(path: List(k), value: v) -> Trie(k, v) {
  insert(new(), at: path, value: value)
}

/// Gets the number of elements in the trie.
/// 
/// ## Examples
/// 
/// ```gleam
/// > [#([1, 2], "a"), #([1], "b")]
/// > |> from_list
/// > |> size
/// 2
/// ```
/// 
pub fn size(trie: Trie(k, v)) -> Int {
  fold(trie, from: 0, with: fn(acc, _, _) { acc + 1 })
}

/// Gets the subtrie whose elements all share a common given prefix.
///
/// ## Examples
///
/// ```gleam
/// > [#([1, 2, 3], "a"), #([1, 2, 4, 5], "b"), #([3, 4], "c")]
/// > |> from_list
/// > |> subtrie(at: [1, 2])
/// > |> to_list
/// [#([1, 2, 3], "a"), #([1, 2, 4, 5], "b")]
/// ```
///
pub fn subtrie(trie: Trie(k, v), at prefix: List(k)) -> Result(Trie(k, v), Nil) {
  case prefix, trie {
    [], _ -> Ok(trie)
    [first, ..rest], Trie(_, children_map) ->
      children_map
      |> map.get(first)
      |> result.try(subtrie(_, rest))
      |> result.map(fn(subtrie) {
        map.new()
        |> map.insert(first, subtrie)
        |> Trie(None, _)
      })
  }
}

/// Turns a trie into a list of path-value pairs.
/// 
/// ## Examples
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> to_list
/// [#([1, 2], "a")]
/// ```
/// 
/// ```gleam
/// > new()
/// > |> to_list
/// []
/// ```
///
pub fn to_list(trie: Trie(k, v)) -> List(#(List(k), v)) {
  fold(
    over: trie,
    from: [],
    with: fn(rest, path, value) { [#(path, value), ..rest] },
  )
}

/// Updates the value associated with a path applying it the given function.
/// If there is no value associated with the given path the function is passed `None`.
/// 
/// If the function returns `None` any value associated with the path is deleted from the trie.
/// If the function returns `Some(value)` then the new value is associated to the given path.
/// 
/// ## Examples
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> update(at: [1, 2], with: fn(n) { n |> option.map(fn(_) { "b" }) })
/// > |> to_list
/// [#([1, 2], "b")] 
/// ```
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> update(at: [1, 2], with: fn(_) { None })
/// > |> to_list
/// []
/// ```
/// 
/// ```gleam
/// > singleton([1, 2], "a")
/// > |> update(at: [1], with: fn(_) { Some("b") })
/// > |> to_list
/// [#([1, 2], "a"), #([1], "b")]
/// ```
/// 
pub fn update(
  trie: Trie(k, v),
  at path: List(k),
  with fun: fn(Option(v)) -> Option(v),
) -> Trie(k, v) {
  do_update(trie, at: path, with: fun)
  |> option.unwrap(new())
}

/// Exactly same behaviour as update but returns `None` if the tree is empty as a
/// result of the (possible) deletion.
///
fn do_update(
  trie: Trie(k, v),
  at path: List(k),
  with fun: fn(Option(v)) -> Option(v),
) -> Option(Trie(k, v)) {
  case path, trie {
    [], Trie(entry, children_map) -> {
      case fun(entry), map.size(children_map) {
        None, 0 -> None
        _ as new_entry, _ -> Some(Trie(new_entry, children_map))
      }
    }
    [first, ..rest], Trie(entry, children_map) -> {
      let new_children = case map.get(children_map, first) {
        Ok(child) ->
          case do_update(child, at: rest, with: fun) {
            None -> map.delete(children_map, first)
            Some(new_child) -> map.insert(children_map, first, new_child)
          }
        Error(_) -> {
          case fun(None) {
            None -> children_map
            Some(value) ->
              map.insert(children_map, first, singleton(rest, value))
          }
        }
      }

      case entry, map.size(new_children) {
        None, 0 -> None
        _, _ -> Some(Trie(entry, new_children))
      }
    }
  }
}

/// Gets a list of all the values in a given trie.
/// 
/// Tries are not ordered so the values are not returned in any specific order.
/// Do not write code that relies on the order values are returned by this function
/// as it may change in later versions of the library.
/// 
/// ## Examples
/// 
/// ```gleam
/// > [#([1, 2], "a"), #([1], "b")]
/// > |> from_list
/// > |> values
/// ["a", "b"]
/// ```
/// 
/// ```gleam
/// > new()
/// > |> values
/// []
/// ```
/// 
pub fn values(trie: Trie(k, v)) -> List(v) {
  fold(trie, from: [], with: fn(values, _, value) { [value, ..values] })
}
