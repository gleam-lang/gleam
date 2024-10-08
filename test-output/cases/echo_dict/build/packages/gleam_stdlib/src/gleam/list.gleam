//// Lists are an ordered sequence of elements and are one of the most common
//// data types in Gleam.
////
//// New elements can be added and removed from the front of a list in
//// constant time, while adding and removing from the end requires traversing
//// and copying the whole list, so keep this in mind when designing your
//// programs.
////
//// There is a dedicated syntax for prefixing to a list:
////
//// ```gleam
//// let new_list = [1, 2, ..existing_list]
//// ```
////
//// And a matching syntax for getting the first elements of a list:
////
//// ```gleam
//// case list {
////   [first_element, ..rest] -> first_element
////   _ -> "this pattern matches when the list is empty"
//// }
//// ```
////

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/order.{type Order}
import gleam/pair

/// Counts the number of elements in a given list.
///
/// This function has to traverse the list to determine the number of elements,
/// so it runs in linear time.
///
/// This function is natively implemented by the virtual machine and is highly
/// optimised.
///
/// ## Examples
///
/// ```gleam
/// length([])
/// // -> 0
/// ```
///
/// ```gleam
/// length([1])
/// // -> 1
/// ```
///
/// ```gleam
/// length([1, 2])
/// // -> 2
/// ```
///
@external(erlang, "erlang", "length")
pub fn length(of list: List(a)) -> Int {
  count_length(list, 0)
}

fn count_length(list: List(a), count: Int) -> Int {
  case list {
    [_, ..list] -> count_length(list, count + 1)
    _ -> count
  }
}

/// Counts the number of elements in a given list satisfying a given predicate.
///
/// This function has to traverse the list to determine the number of elements,
/// so it runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// count([], fn(a) { a > 0 })
/// // -> 0
/// ```
///
/// ```gleam
/// count([1], fn(a) { a > 0 })
/// // -> 1
/// ```
///
/// ```gleam
/// count([1, 2, 3], int.is_odd)
/// // -> 2
/// ```
///
pub fn count(list: List(a), where predicate: fn(a) -> Bool) -> Int {
  fold(list, 0, fn(acc, value) {
    case predicate(value) {
      True -> acc + 1
      False -> acc
    }
  })
}

/// Creates a new list from a given list containing the same elements but in the
/// opposite order.
///
/// This function has to traverse the list to create the new reversed list, so
/// it runs in linear time.
///
/// This function is natively implemented by the virtual machine and is highly
/// optimised.
///
/// ## Examples
///
/// ```gleam
/// reverse([])
/// // -> []
/// ```
///
/// ```gleam
/// reverse([1])
/// // -> [1]
/// ```
///
/// ```gleam
/// reverse([1, 2])
/// // -> [2, 1]
/// ```
///
@external(erlang, "lists", "reverse")
pub fn reverse(list: List(a)) -> List(a) {
  do_reverse(list, [])
}

fn do_reverse(remaining: List(a), accumulator: List(a)) -> List(a) {
  case remaining {
    [] -> accumulator
    [item, ..rest] -> do_reverse(rest, [item, ..accumulator])
  }
}

/// Determines whether or not the list is empty.
///
/// This function runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// is_empty([])
/// // -> True
/// ```
///
/// ```gleam
/// is_empty([1])
/// // -> False
/// ```
///
/// ```gleam
/// is_empty([1, 1])
/// // -> False
/// ```
///
pub fn is_empty(list: List(a)) -> Bool {
  list == []
}

/// Determines whether or not a given element exists within a given list.
///
/// This function traverses the list to find the element, so it runs in linear
/// time.
///
/// ## Examples
///
/// ```gleam
/// [] |> contains(any: 0)
/// // -> False
/// ```
///
/// ```gleam
/// [0] |> contains(any: 0)
/// // -> True
/// ```
///
/// ```gleam
/// [1] |> contains(any: 0)
/// // -> False
/// ```
///
/// ```gleam
/// [1, 1] |> contains(any: 0)
/// // -> False
/// ```
///
/// ```gleam
/// [1, 0] |> contains(any: 0)
/// // -> True
/// ```
///
pub fn contains(list: List(a), any elem: a) -> Bool {
  case list {
    [] -> False
    [first, ..] if first == elem -> True
    [_, ..rest] -> contains(rest, elem)
  }
}

/// Gets the first element from the start of the list, if there is one.
///
/// ## Examples
///
/// ```gleam
/// first([])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// first([0])
/// // -> Ok(0)
/// ```
///
/// ```gleam
/// first([1, 2])
/// // -> Ok(1)
/// ```
///
pub fn first(list: List(a)) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [x, ..] -> Ok(x)
  }
}

/// Returns the list minus the first element. If the list is empty, `Error(Nil)` is
/// returned.
///
/// This function runs in constant time and does not make a copy of the list.
///
/// ## Examples
///
/// ```gleam
/// rest([])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// rest([0])
/// // -> Ok([])
/// ```
///
/// ```gleam
/// rest([1, 2])
/// // -> Ok([2])
/// ```
///
pub fn rest(list: List(a)) -> Result(List(a), Nil) {
  case list {
    [] -> Error(Nil)
    [_, ..rest] -> Ok(rest)
  }
}

fn update_group(f: fn(a) -> k) -> fn(Dict(k, List(a)), a) -> Dict(k, List(a)) {
  fn(groups, elem) {
    case dict.get(groups, f(elem)) {
      Ok(existing) -> dict.insert(groups, f(elem), [elem, ..existing])
      Error(_) -> dict.insert(groups, f(elem), [elem])
    }
  }
}

/// Takes a list and groups the values by a key
/// which is built from a key function.
///
/// Does not preserve the initial value order.
///
/// ## Examples
///
/// ```gleam
/// import gleam/dict
///
/// [Ok(3), Error("Wrong"), Ok(200), Ok(73)]
/// |> group(by: fn(i) {
///   case i {
///     Ok(_) -> "Successful"
///     Error(_) -> "Failed"
///   }
/// })
/// |> dict.to_list
/// // -> [
/// //   #("Failed", [Error("Wrong")]),
/// //   #("Successful", [Ok(73), Ok(200), Ok(3)])
/// // ]
/// ```
///
/// ```gleam
/// import gleam/dict
///
/// group([1,2,3,4,5], by: fn(i) { i - i / 3 * 3 })
/// |> dict.to_list
/// // -> [#(0, [3]), #(1, [4, 1]), #(2, [5, 2])]
/// ```
///
pub fn group(list: List(v), by key: fn(v) -> k) -> Dict(k, List(v)) {
  fold(list, dict.new(), update_group(key))
}

fn do_filter(list: List(a), fun: fn(a) -> Bool, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> {
      let new_acc = case fun(first) {
        True -> [first, ..acc]
        False -> acc
      }
      do_filter(rest, fun, new_acc)
    }
  }
}

/// Returns a new list containing only the elements from the first list for
/// which the given functions returns `True`.
///
/// ## Examples
///
/// ```gleam
/// filter([2, 4, 6, 1], fn(x) { x > 2 })
/// // -> [4, 6]
/// ```
///
/// ```gleam
/// filter([2, 4, 6, 1], fn(x) { x > 6 })
/// // -> []
/// ```
///
pub fn filter(list: List(a), keeping predicate: fn(a) -> Bool) -> List(a) {
  do_filter(list, predicate, [])
}

fn do_filter_map(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> {
      let new_acc = case fun(first) {
        Ok(first) -> [first, ..acc]
        Error(_) -> acc
      }
      do_filter_map(rest, fun, new_acc)
    }
  }
}

/// Returns a new list containing only the elements from the first list for
/// which the given functions returns `Ok(_)`.
///
/// ## Examples
///
/// ```gleam
/// filter_map([2, 4, 6, 1], Error)
/// // -> []
/// ```
///
/// ```gleam
/// filter_map([2, 4, 6, 1], fn(x) { Ok(x + 1) })
/// // -> [3, 5, 7, 2]
/// ```
///
pub fn filter_map(list: List(a), with fun: fn(a) -> Result(b, e)) -> List(b) {
  do_filter_map(list, fun, [])
}

fn do_map(list: List(a), fun: fn(a) -> b, acc: List(b)) -> List(b) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> do_map(rest, fun, [fun(first), ..acc])
  }
}

/// Returns a new list containing only the elements of the first list after the
/// function has been applied to each one.
///
/// ## Examples
///
/// ```gleam
/// map([2, 4, 6], fn(x) { x * 2 })
/// // -> [4, 8, 12]
/// ```
///
pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) {
  do_map(list, fun, [])
}

/// Combines two lists into a single list using the given function.
///
/// If a list is longer than the other the extra elements are dropped.
///
/// ## Examples
///
/// ```gleam
/// map2([1, 2, 3], [4, 5, 6], fn(x, y) { x + y })
/// // -> [5, 7, 9]
/// ```
///
/// ```gleam
/// map2([1, 2], ["a", "b", "c"], fn(i, x) { #(i, x) })
/// // -> [#(1, "a"), #(2, "b")]
/// ```
///
pub fn map2(list1: List(a), list2: List(b), with fun: fn(a, b) -> c) -> List(c) {
  do_map2(list1, list2, fun, [])
}

fn do_map2(
  list1: List(a),
  list2: List(b),
  fun: fn(a, b) -> c,
  acc: List(c),
) -> List(c) {
  case list1, list2 {
    [], _ | _, [] -> reverse(acc)
    [a, ..as_], [b, ..bs] -> do_map2(as_, bs, fun, [fun(a, b), ..acc])
  }
}

/// Similar to `map` but also lets you pass around an accumulated value.
///
/// ## Examples
///
/// ```gleam
/// map_fold(
///   over: [1, 2, 3],
///   from: 100,
///   with: fn(memo, i) { #(memo + i, i * 2) }
/// )
/// // -> #(106, [2, 4, 6])
/// ```
///
pub fn map_fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> #(acc, b),
) -> #(acc, List(b)) {
  fold(over: list, from: #(initial, []), with: fn(acc, item) {
    let #(current_acc, items) = acc
    let #(next_acc, next_item) = fun(current_acc, item)
    #(next_acc, [next_item, ..items])
  })
  |> pair.map_second(reverse)
}

fn do_index_map(
  list: List(a),
  fun: fn(a, Int) -> b,
  index: Int,
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] -> {
      let acc = [fun(first, index), ..acc]
      do_index_map(rest, fun, index + 1, acc)
    }
  }
}

/// Returns a new list containing only the elements of the first list after the
/// function has been applied to each one and their index.
///
/// The index starts at 0, so the first element is 0, the second is 1, and so
/// on.
///
/// ## Examples
///
/// ```gleam
/// index_map(["a", "b"], fn(x, i) { #(i, x) })
/// // -> [#(0, "a"), #(1, "b")]
/// ```
///
pub fn index_map(list: List(a), with fun: fn(a, Int) -> b) -> List(b) {
  do_index_map(list, fun, 0, [])
}

fn do_try_map(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> Result(List(b), e) {
  case list {
    [] -> Ok(reverse(acc))
    [first, ..rest] ->
      case fun(first) {
        Ok(first) -> do_try_map(rest, fun, [first, ..acc])
        Error(error) -> Error(error)
      }
  }
}

/// Takes a function that returns a `Result` and applies it to each element in a
/// given list in turn.
///
/// If the function returns `Ok(new_value)` for all elements in the list then a
/// list of the new values is returned.
///
/// If the function returns `Error(reason)` for any of the elements then it is
/// returned immediately. None of the elements in the list are processed after
/// one returns an `Error`.
///
/// ## Examples
///
/// ```gleam
/// try_map([1, 2, 3], fn(x) { Ok(x + 2) })
/// // -> Ok([3, 4, 5])
/// ```
///
/// ```gleam
/// try_map([1, 2, 3], fn(_) { Error(0) })
/// // -> Error(0)
/// ```
///
/// ```gleam
/// try_map([[1], [2, 3]], first)
/// // -> Ok([1, 2])
/// ```
///
/// ```gleam
/// try_map([[1], [], [2]], first)
/// // -> Error(Nil)
/// ```
///
pub fn try_map(
  over list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(List(b), e) {
  do_try_map(list, fun, [])
}

/// Returns a list that is the given list with up to the given number of
/// elements removed from the front of the list.
///
/// If the element has less than the number of elements an empty list is
/// returned.
///
/// This function runs in linear time but does not copy the list.
///
/// ## Examples
///
/// ```gleam
/// drop([1, 2, 3, 4], 2)
/// // -> [3, 4]
/// ```
///
/// ```gleam
/// drop([1, 2, 3, 4], 9)
/// // -> []
/// ```
///
pub fn drop(from list: List(a), up_to n: Int) -> List(a) {
  case n <= 0 {
    True -> list
    False ->
      case list {
        [] -> []
        [_, ..rest] -> drop(rest, n - 1)
      }
  }
}

fn do_take(list: List(a), n: Int, acc: List(a)) -> List(a) {
  case n <= 0 {
    True -> reverse(acc)
    False ->
      case list {
        [] -> reverse(acc)
        [first, ..rest] -> do_take(rest, n - 1, [first, ..acc])
      }
  }
}

/// Returns a list containing the first given number of elements from the given
/// list.
///
/// If the element has less than the number of elements then the full list is
/// returned.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// take([1, 2, 3, 4], 2)
/// // -> [1, 2]
/// ```
///
/// ```gleam
/// take([1, 2, 3, 4], 9)
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn take(from list: List(a), up_to n: Int) -> List(a) {
  do_take(list, n, [])
}

/// Returns a new empty list.
///
/// ## Examples
///
/// ```gleam
/// new()
/// // -> []
/// ```
///
pub fn new() -> List(a) {
  []
}

/// Returns the given item wrapped in a list.
///
/// ## Examples
///
/// ```gleam
/// wrap(1)
/// // -> [1]
///
/// wrap(["a", "b", "c"])
/// // -> [["a", "b", "c"]]
///
/// wrap([[]])
/// // -> [[[]]]
/// ```
///
///
pub fn wrap(item: a) -> List(a) {
  [item]
}

/// Joins one list onto the end of another.
///
/// This function runs in linear time, and it traverses and copies the first
/// list.
///
/// ## Examples
///
/// ```gleam
/// append([1, 2], [3])
/// // -> [1, 2, 3]
/// ```
///
@external(erlang, "lists", "append")
pub fn append(first: List(a), second: List(a)) -> List(a) {
  do_append(reverse(first), second)
}

fn do_append(first: List(a), second: List(a)) -> List(a) {
  case first {
    [] -> second
    [item, ..rest] -> do_append(rest, [item, ..second])
  }
}

/// Prefixes an item to a list. This can also be done using the dedicated
/// syntax instead
///
/// ```gleam
/// let existing_list = [2, 3, 4]
///
/// [1, ..existing_list]
/// // -> [1, 2, 3, 4]
///
/// prepend(to: existing_list, this: 1)
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn prepend(to list: List(a), this item: a) -> List(a) {
  [item, ..list]
}

// Reverses a list and prepends it to another list
fn reverse_and_prepend(list prefix: List(a), to suffix: List(a)) -> List(a) {
  case prefix {
    [] -> suffix
    [first, ..rest] -> reverse_and_prepend(list: rest, to: [first, ..suffix])
  }
}

fn do_concat(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> reverse(acc)
    [list, ..further_lists] ->
      do_concat(further_lists, reverse_and_prepend(list: list, to: acc))
  }
}

/// Joins a list of lists into a single list.
///
/// This function traverses all elements twice.
///
/// ## Examples
///
/// ```gleam
/// concat([[1], [2, 3], []])
/// // -> [1, 2, 3]
/// ```
///
@deprecated("Use `list.flatten` instead.")
pub fn concat(lists: List(List(a))) -> List(a) {
  do_concat(lists, [])
}

/// This is the same as `concat`: it joins a list of lists into a single
/// list.
///
/// This function traverses all elements twice.
///
/// ## Examples
///
/// ```gleam
/// flatten([[1], [2, 3], []])
/// // -> [1, 2, 3]
/// ```
///
pub fn flatten(lists: List(List(a))) -> List(a) {
  do_concat(lists, [])
}

/// Maps the list with the given function into a list of lists, and then flattens it.
///
/// ## Examples
///
/// ```gleam
/// flat_map([2, 4, 6], fn(x) { [x, x + 1] })
/// // -> [2, 3, 4, 5, 6, 7]
/// ```
///
pub fn flat_map(over list: List(a), with fun: fn(a) -> List(b)) -> List(b) {
  map(list, fun)
  |> flatten
}

/// Reduces a list of elements into a single value by calling a given function
/// on each element, going from left to right.
///
/// `fold([1, 2, 3], 0, add)` is the equivalent of
/// `add(add(add(0, 1), 2), 3)`.
///
/// This function runs in linear time.
///
pub fn fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> acc {
  case list {
    [] -> initial
    [x, ..rest] -> fold(rest, fun(initial, x), fun)
  }
}

/// Reduces a list of elements into a single value by calling a given function
/// on each element, going from right to left.
///
/// `fold_right([1, 2, 3], 0, add)` is the equivalent of
/// `add(add(add(0, 3), 2), 1)`.
///
/// This function runs in linear time.
///
/// Unlike `fold` this function is not tail recursive. Where possible use
/// `fold` instead as it will use less memory.
///
pub fn fold_right(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> acc {
  case list {
    [] -> initial
    [x, ..rest] -> fun(fold_right(rest, initial, fun), x)
  }
}

fn do_index_fold(
  over: List(a),
  acc: acc,
  with: fn(acc, a, Int) -> acc,
  index: Int,
) -> acc {
  case over {
    [] -> acc
    [first, ..rest] ->
      do_index_fold(rest, with(acc, first, index), with, index + 1)
  }
}

/// Like fold but the folding function also receives the index of the current element.
///
/// ## Examples
///
/// ```gleam
/// ["a", "b", "c"]
/// |> index_fold([], fn(acc, item, index) { ... })
/// ```
///
pub fn index_fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a, Int) -> acc,
) -> acc {
  do_index_fold(list, initial, fun, 0)
}

/// A variant of fold that might fail.
///
/// The folding function should return `Result(accumulator, error)`.
/// If the returned value is `Ok(accumulator)` try_fold will try the next value in the list.
/// If the returned value is `Error(error)` try_fold will stop and return that error.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 3, 4]
/// |> try_fold(0, fn(acc, i) {
///   case i < 3 {
///     True -> Ok(acc + i)
///     False -> Error(Nil)
///   }
/// })
/// // -> Error(Nil)
/// ```
///
pub fn try_fold(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> Result(acc, e),
) -> Result(acc, e) {
  case list {
    [] -> Ok(initial)
    [first, ..rest] ->
      case fun(initial, first) {
        Ok(result) -> try_fold(rest, result, fun)
        Error(_) as error -> error
      }
  }
}

pub type ContinueOrStop(a) {
  Continue(a)
  Stop(a)
}

/// A variant of fold that allows to stop folding earlier.
///
/// The folding function should return `ContinueOrStop(accumulator)`.
/// If the returned value is `Continue(accumulator)` fold_until will try the next value in the list.
/// If the returned value is `Stop(accumulator)` fold_until will stop and return that accumulator.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 3, 4]
/// |> fold_until(0, fn(acc, i) {
///   case i < 3 {
///     True -> Continue(acc + i)
///     False -> Stop(acc)
///   }
/// })
/// // -> 3
/// ```
///
pub fn fold_until(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> ContinueOrStop(acc),
) -> acc {
  case list {
    [] -> initial
    [first, ..rest] ->
      case fun(initial, first) {
        Continue(next_accumulator) -> fold_until(rest, next_accumulator, fun)
        Stop(b) -> b
      }
  }
}

/// Finds the first element in a given list for which the given function returns
/// `True`.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// find([1, 2, 3], fn(x) { x > 2 })
/// // -> Ok(3)
/// ```
///
/// ```gleam
/// find([1, 2, 3], fn(x) { x > 4 })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find([], fn(_) { True })
/// // -> Error(Nil)
/// ```
///
pub fn find(
  in list: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [x, ..rest] ->
      case is_desired(x) {
        True -> Ok(x)
        _ -> find(in: rest, one_that: is_desired)
      }
  }
}

/// Finds the first element in a given list for which the given function returns
/// `Ok(new_value)`, then returns the wrapped `new_value`.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// find_map([[], [2], [3]], first)
/// // -> Ok(2)
/// ```
///
/// ```gleam
/// find_map([[], []], first)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find_map([], first)
/// // -> Error(Nil)
/// ```
///
pub fn find_map(
  in list: List(a),
  with fun: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  case list {
    [] -> Error(Nil)
    [x, ..rest] ->
      case fun(x) {
        Ok(x) -> Ok(x)
        _ -> find_map(in: rest, with: fun)
      }
  }
}

/// Returns `True` if the given function returns `True` for all the elements in
/// the given list. If the function returns `False` for any of the elements it
/// immediately returns `False` without checking the rest of the list.
///
/// ## Examples
///
/// ```gleam
/// all([], fn(x) { x > 3 })
/// // -> True
/// ```
///
/// ```gleam
/// all([4, 5], fn(x) { x > 3 })
/// // -> True
/// ```
///
/// ```gleam
/// all([4, 3], fn(x) { x > 3 })
/// // -> False
/// ```
///
pub fn all(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> True
    [first, ..rest] ->
      case predicate(first) {
        True -> all(rest, predicate)
        False -> False
      }
  }
}

/// Returns `True` if the given function returns `True` for any the elements in
/// the given list. If the function returns `True` for any of the elements it
/// immediately returns `True` without checking the rest of the list.
///
/// ## Examples
///
/// ```gleam
/// any([], fn(x) { x > 3 })
/// // -> False
/// ```
///
/// ```gleam
/// any([4, 5], fn(x) { x > 3 })
/// // -> True
/// ```
///
/// ```gleam
/// any([4, 3], fn(x) { x > 4 })
/// // -> False
/// ```
///
/// ```gleam
/// any([3, 4], fn(x) { x > 3 })
/// // -> True
/// ```
///
pub fn any(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> False
    [first, ..rest] ->
      case predicate(first) {
        True -> True
        False -> any(rest, predicate)
      }
  }
}

fn do_zip(one: List(a), other: List(b), acc: List(#(a, b))) -> List(#(a, b)) {
  case one, other {
    [first_one, ..rest_one], [first_other, ..rest_other] ->
      do_zip(rest_one, rest_other, [#(first_one, first_other), ..acc])
    _, _ -> reverse(acc)
  }
}

/// Takes two lists and returns a single list of 2-element tuples.
///
/// If one of the lists is longer than the other, the remaining elements from
/// the longer list are not used.
///
/// ## Examples
///
/// ```gleam
/// zip([], [])
/// // -> []
/// ```
///
/// ```gleam
/// zip([1, 2], [3])
/// // -> [#(1, 3)]
/// ```
///
/// ```gleam
/// zip([1], [3, 4])
/// // -> [#(1, 3)]
/// ```
///
/// ```gleam
/// zip([1, 2], [3, 4])
/// // -> [#(1, 3), #(2, 4)]
/// ```
///
pub fn zip(list: List(a), with other: List(b)) -> List(#(a, b)) {
  do_zip(list, other, [])
}

/// Takes two lists and returns a single list of 2-element tuples.
///
/// If one of the lists is longer than the other, an `Error` is returned.
///
/// ## Examples
///
/// ```gleam
/// strict_zip([], [])
/// // -> Ok([])
/// ```
///
/// ```gleam
/// strict_zip([1, 2], [3])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// strict_zip([1], [3, 4])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// strict_zip([1, 2], [3, 4])
/// // -> Ok([#(1, 3), #(2, 4)])
/// ```
///
pub fn strict_zip(
  list: List(a),
  with other: List(b),
) -> Result(List(#(a, b)), Nil) {
  case length(of: list) == length(of: other) {
    True -> Ok(zip(list, other))
    False -> Error(Nil)
  }
}

fn do_unzip(
  input: List(#(a, b)),
  one: List(a),
  other: List(b),
) -> #(List(a), List(b)) {
  case input {
    [] -> #(reverse(one), reverse(other))
    [#(first_one, first_other), ..rest] ->
      do_unzip(rest, [first_one, ..one], [first_other, ..other])
  }
}

/// Takes a single list of 2-element tuples and returns two lists.
///
/// ## Examples
///
/// ```gleam
/// unzip([#(1, 2), #(3, 4)])
/// // -> #([1, 3], [2, 4])
/// ```
///
/// ```gleam
/// unzip([])
/// // -> #([], [])
/// ```
///
pub fn unzip(input: List(#(a, b))) -> #(List(a), List(b)) {
  do_unzip(input, [], [])
}

fn do_intersperse(list: List(a), separator: a, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [x, ..rest] -> do_intersperse(rest, separator, [x, separator, ..acc])
  }
}

/// Inserts a given value between each existing element in a given list.
///
/// This function runs in linear time and copies the list.
///
/// ## Examples
///
/// ```gleam
/// intersperse([1, 1, 1], 2)
/// // -> [1, 2, 1, 2, 1]
/// ```
///
/// ```gleam
/// intersperse([], 2)
/// // -> []
/// ```
///
pub fn intersperse(list: List(a), with elem: a) -> List(a) {
  case list {
    [] | [_] -> list
    [x, ..rest] -> do_intersperse(rest, elem, [x])
  }
}

/// Removes any duplicate elements from a given list.
///
/// This function returns in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// unique([1, 1, 1, 4, 7, 3, 3, 4])
/// // -> [1, 4, 7, 3]
/// ```
///
pub fn unique(list: List(a)) -> List(a) {
  case list {
    [] -> []
    [x, ..rest] -> [x, ..unique(filter(rest, fn(y) { y != x }))]
  }
}

/// Sorts from smallest to largest based upon the ordering specified by a given
/// function.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// sort([4, 3, 6, 5, 4, 1, 2], by: int.compare)
/// // -> [1, 2, 3, 4, 4, 5, 6]
/// ```
///
pub fn sort(list: List(a), by compare: fn(a, a) -> Order) -> List(a) {
  // This is a natural, tail recursive, stable merge sort:
  // - natural: it is very efficient if you call it on a list that is already
  //   (pre)sorted because it works on slices of the original list.
  // - tail recursive: the stack won't grow linearly with the size of the list.
  // - stable: if two items are considered to be equal then their original
  //   relative order is preserved.
  case list {
    // If the list has zero/one item then it's already sorted.
    [] -> []
    [x] -> [x]

    // Otherwise the algorithm works as follow: we split the list in sequences
    // of already sorted values as they appear in the list and then we merge
    // those together two by two using `merge_all`.
    [x, y, ..rest] -> {
      // We need to compare the first two items to properly call `sequences`
      // with the correct initial values. If the second item is <= than the
      // first, then we know we'll start by growing a descending sequence
      // (and an ascending one in the opposite case).
      let direction = case compare(x, y) {
        order.Lt | order.Eq -> Ascending
        order.Gt -> Descending
      }

      // `sequences` produces sequences in ascending order so we call the
      // `merge_all` function saying it to expect all sequences to be sorted
      // that way.
      let sequences = sequences(rest, compare, [x], direction, y, [])
      merge_all(sequences, Ascending, compare)
    }
  }
}

type Sorting {
  Ascending
  Descending
}

/// Given a list it returns slices of it that are locally sorted in ascending
/// order.
///
/// Imagine you have this list:
///
/// ```
///   [1, 2, 3, 2, 1, 0]
///    ^^^^^^^  ^^^^^^^ This is a slice in descending order
///    |
///    | This is a slice that is sorted in ascending order
/// ```
///
/// So the produced result will contain these two slices, each one sorted in
/// ascending order: `[[1, 2, 3], [0, 1, 2]]`.
///
/// - `growing` is an accumulator with the current slice being grown
/// - `direction` is the growing direction of the slice being grown, it could
///   either be ascending or strictly descending
/// - `prev` is the previous element that needs to be added to the growing slice
///   it is carried around to check whether we have to keep growing the current
///   slice or not
/// - `acc` is the accumulator containing the slices sorted in ascending order
///
fn sequences(
  list: List(a),
  compare: fn(a, a) -> Order,
  growing: List(a),
  direction: Sorting,
  prev: a,
  acc: List(List(a)),
) -> List(List(a)) {
  // First of all we must not forget to add the previous element to the
  // currently growing slice.
  let growing = [prev, ..growing]

  case list {
    [] ->
      case direction {
        // Notice how we have to reverse the accumulator we're growing: since
        // we always add items to the head, `growing` is built in the opposite
        // sorting order of what it actually is in the original list.
        Ascending -> [do_reverse(growing, []), ..acc]
        Descending -> [growing, ..acc]
      }

    [new, ..rest] ->
      case compare(prev, new), direction {
        // In case the new element respects the ordering of the growing
        // sequence, then we just keep growing it.
        // Notice how a growing sequence is weakly growing (that is it can have
        // consecutive equal items) while a decreasing sequence is strictly
        // decreasing (no consecutive equal items), this is needed to make the
        // algorithm stable!
        order.Gt, Descending | order.Lt, Ascending | order.Eq, Ascending ->
          sequences(rest, compare, growing, direction, new, acc)

        // We were growing an ascending (descending) sequence and the new item
        // is smaller (bigger) than the previous one, this means we have to stop
        // growing this sequence and start with a new one whose first item will
        // be the one we just found.
        order.Gt, Ascending | order.Lt, Descending | order.Eq, Descending -> {
          let acc = case direction {
            Ascending -> [do_reverse(growing, []), ..acc]
            Descending -> [growing, ..acc]
          }
          case rest {
            // The list is over so we just create a sequence containing the last
            // item we saw and add it to the accumulator before returning it.
            [] -> [[new], ..acc]

            // If the list is not over we have a peek at the next item to decide
            // in which direction is growing the new sequence and make the
            // recursive call with the appropriate arguments.
            [next, ..rest] -> {
              let direction = case compare(new, next) {
                order.Lt | order.Eq -> Ascending
                order.Gt -> Descending
              }
              sequences(rest, compare, [new], direction, next, acc)
            }
          }
        }
      }
  }
}

/// Given some some sorted sequences (assumed to be sorted in `direction`) it
/// merges them all together until we're left with just a list sorted in
/// ascending order.
///
fn merge_all(
  sequences: List(List(a)),
  direction: Sorting,
  compare: fn(a, a) -> Order,
) -> List(a) {
  case sequences, direction {
    [], _ -> []

    // If we have a single list in ascending order then we're done.
    [sequence], Ascending -> sequence

    // If we have a single list in descending order, we reverse it to make sure
    // it's in ascending order and we're done.
    [sequence], Descending -> do_reverse(sequence, [])

    // Merging together sequences that are in ascending (descending) order
    // reverses their order, so the recursive call will assume to be merging
    // lists sorted in the opposite order!
    _, Ascending -> {
      let sequences = merge_ascending_pairs(sequences, compare, [])
      merge_all(sequences, Descending, compare)
    }

    _, Descending -> {
      let sequences = merge_descending_pairs(sequences, compare, [])
      merge_all(sequences, Ascending, compare)
    }
  }
}

/// Given a list of ascending lists, it merges adjacent pairs into a single
/// descending list, halving their number.
/// It returns a list of the remaining descending lists.
///
fn merge_ascending_pairs(
  sequences: List(List(a)),
  compare: fn(a, a) -> Order,
  acc: List(List(a)),
) {
  case sequences {
    [] -> do_reverse(acc, [])

    // Beware, if we have just one item left we must reverse it: we take
    // ascending lists as input and have to return descending ones.
    // If we returned it like it is it would be sorted in ascending order.
    [sequence] -> do_reverse([do_reverse(sequence, []), ..acc], [])

    [ascending1, ascending2, ..rest] -> {
      let descending = merge_ascendings(ascending1, ascending2, compare, [])
      merge_ascending_pairs(rest, compare, [descending, ..acc])
    }
  }
}

/// This is the same as merge_ascending_pairs but flipped for descending lists.
///
fn merge_descending_pairs(
  sequences: List(List(a)),
  compare: fn(a, a) -> Order,
  acc: List(List(a)),
) {
  case sequences {
    [] -> do_reverse(acc, [])

    [sequence] -> do_reverse([do_reverse(sequence, []), ..acc], [])

    [descending1, descending2, ..rest] -> {
      let ascending = merge_descendings(descending1, descending2, compare, [])
      merge_descending_pairs(rest, compare, [ascending, ..acc])
    }
  }
}

/// Merges two lists sorted in ascending order into a single list sorted in
/// descending order according to the given comparator function.
///
/// This reversing of the sort order is not avoidable if we want to implement
/// merge as a tail recursive function. We could reverse the accumulator before
/// returning it but that would end up being less efficient; so the merging
/// algorithm has to play around this.
///
fn merge_ascendings(
  list1: List(a),
  list2: List(a),
  compare: fn(a, a) -> Order,
  acc: List(a),
) -> List(a) {
  case list1, list2 {
    [], list | list, [] -> do_reverse(list, acc)

    [first1, ..rest1], [first2, ..rest2] ->
      case compare(first1, first2) {
        order.Lt -> merge_ascendings(rest1, list2, compare, [first1, ..acc])
        order.Gt | order.Eq ->
          merge_ascendings(list1, rest2, compare, [first2, ..acc])
      }
  }
}

/// This is exactly the same as merge_ascendings but mirrored: it merges two
/// lists sorted in descending order into a single list sorted in ascending
/// order according to the given comparator function.
///
/// This reversing of the sort order is not avoidable if we want to implement
/// merge as a tail recursive function. We could reverse the accumulator before
/// returning it but that would end up being less efficient; so the merging
/// algorithm has to play around this.
///
fn merge_descendings(
  list1: List(a),
  list2: List(a),
  compare: fn(a, a) -> Order,
  acc: List(a),
) -> List(a) {
  case list1, list2 {
    [], list | list, [] -> do_reverse(list, acc)
    [first1, ..rest1], [first2, ..rest2] ->
      case compare(first1, first2) {
        order.Lt -> merge_descendings(list1, rest2, compare, [first2, ..acc])
        order.Gt | order.Eq ->
          merge_descendings(rest1, list2, compare, [first1, ..acc])
      }
  }
}

/// Creates a list of ints ranging from a given start and finish.
///
/// ## Examples
///
/// ```gleam
/// range(0, 0)
/// // -> [0]
/// ```
///
/// ```gleam
/// range(0, 5)
/// // -> [0, 1, 2, 3, 4, 5]
/// ```
///
/// ```gleam
/// range(1, -5)
/// // -> [1, 0, -1, -2, -3, -4, -5]
/// ```
///
pub fn range(from start: Int, to stop: Int) -> List(Int) {
  tail_recursive_range(start, stop, [])
}

fn tail_recursive_range(start: Int, stop: Int, acc: List(Int)) -> List(Int) {
  case int.compare(start, stop) {
    order.Eq -> [stop, ..acc]
    order.Gt -> tail_recursive_range(start, stop + 1, [stop, ..acc])
    order.Lt -> tail_recursive_range(start, stop - 1, [stop, ..acc])
  }
}

fn do_repeat(item: a, times: Int, acc: List(a)) -> List(a) {
  case times <= 0 {
    True -> acc
    False -> do_repeat(item, times - 1, [item, ..acc])
  }
}

/// Builds a list of a given value a given number of times.
///
/// ## Examples
///
/// ```gleam
/// repeat("a", times: 0)
/// // -> []
/// ```
///
/// ```gleam
/// repeat("a", times: 5)
/// // -> ["a", "a", "a", "a", "a"]
/// ```
///
pub fn repeat(item a: a, times times: Int) -> List(a) {
  do_repeat(a, times, [])
}

fn do_split(list: List(a), n: Int, taken: List(a)) -> #(List(a), List(a)) {
  case n <= 0 {
    True -> #(reverse(taken), list)
    False ->
      case list {
        [] -> #(reverse(taken), [])
        [first, ..rest] -> do_split(rest, n - 1, [first, ..taken])
      }
  }
}

/// Splits a list in two before the given index.
///
/// If the list is not long enough to have the given index the before list will
/// be the input list, and the after list will be empty.
///
/// ## Examples
///
/// ```gleam
/// split([6, 7, 8, 9], 0)
/// // -> #([], [6, 7, 8, 9])
/// ```
///
/// ```gleam
/// split([6, 7, 8, 9], 2)
/// // -> #([6, 7], [8, 9])
/// ```
///
/// ```gleam
/// split([6, 7, 8, 9], 4)
/// // -> #([6, 7, 8, 9], [])
/// ```
///
pub fn split(list list: List(a), at index: Int) -> #(List(a), List(a)) {
  do_split(list, index, [])
}

fn do_split_while(
  list: List(a),
  f: fn(a) -> Bool,
  acc: List(a),
) -> #(List(a), List(a)) {
  case list {
    [] -> #(reverse(acc), [])
    [first, ..rest] ->
      case f(first) {
        False -> #(reverse(acc), list)
        _ -> do_split_while(rest, f, [first, ..acc])
      }
  }
}

/// Splits a list in two before the first element that a given function returns
/// `False` for.
///
/// If the function returns `True` for all elements the first list will be the
/// input list, and the second list will be empty.
///
/// ## Examples
///
/// ```gleam
/// split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })
/// // -> #([1, 2, 3], [4, 5])
/// ```
///
/// ```gleam
/// split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })
/// // -> #([1, 2, 3, 4, 5], [])
/// ```
///
pub fn split_while(
  list list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  do_split_while(list, predicate, [])
}

/// Given a list of 2-element tuples, finds the first tuple that has a given
/// key as the first element and returns the second element.
///
/// If no tuple is found with the given key then `Error(Nil)` is returned.
///
/// This function may be useful for interacting with Erlang code where lists of
/// tuples are common.
///
/// ## Examples
///
/// ```gleam
/// key_find([#("a", 0), #("b", 1)], "a")
/// // -> Ok(0)
/// ```
///
/// ```gleam
/// key_find([#("a", 0), #("b", 1)], "b")
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// key_find([#("a", 0), #("b", 1)], "c")
/// // -> Error(Nil)
/// ```
///
pub fn key_find(
  in keyword_list: List(#(k, v)),
  find desired_key: k,
) -> Result(v, Nil) {
  find_map(keyword_list, fn(keyword) {
    let #(key, value) = keyword
    case key == desired_key {
      True -> Ok(value)
      False -> Error(Nil)
    }
  })
}

/// Given a list of 2-element tuples, finds all tuples that have a given
/// key as the first element and returns the second element.
///
/// This function may be useful for interacting with Erlang code where lists of
/// tuples are common.
///
/// ## Examples
///
/// ```gleam
/// key_filter([#("a", 0), #("b", 1), #("a", 2)], "a")
/// // -> [0, 2]
/// ```
///
/// ```gleam
/// key_filter([#("a", 0), #("b", 1)], "c")
/// // -> []
/// ```
///
pub fn key_filter(
  in keyword_list: List(#(k, v)),
  find desired_key: k,
) -> List(v) {
  filter_map(keyword_list, fn(keyword) {
    let #(key, value) = keyword
    case key == desired_key {
      True -> Ok(value)
      False -> Error(Nil)
    }
  })
}

fn do_pop(haystack, predicate, checked) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case predicate(x) {
        True -> Ok(#(x, append(reverse(checked), rest)))
        False -> do_pop(rest, predicate, [x, ..checked])
      }
  }
}

/// Removes the first element in a given list for which the predicate function returns `True`.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// pop([1, 2, 3], fn(x) { x > 2 })
/// // -> Ok(#(3, [1, 2]))
/// ```
///
/// ```gleam
/// pop([1, 2, 3], fn(x) { x > 4 })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// pop([], fn(_) { True })
/// // -> Error(Nil)
/// ```
///
pub fn pop(
  in list: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(#(a, List(a)), Nil) {
  do_pop(list, is_desired, [])
}

fn do_pop_map(
  list: List(a),
  mapper: fn(a) -> Result(b, e),
  checked: List(a),
) -> Result(#(b, List(a)), Nil) {
  case list {
    [] -> Error(Nil)
    [x, ..rest] ->
      case mapper(x) {
        Ok(y) -> Ok(#(y, append(reverse(checked), rest)))
        Error(_) -> do_pop_map(rest, mapper, [x, ..checked])
      }
  }
}

/// Removes the first element in a given list for which the given function returns
/// `Ok(new_value)`, then returns the wrapped `new_value` as well as list with the value removed.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// pop_map([[], [2], [3]], first)
/// // -> Ok(#(2, [[], [3]]))
/// ```
///
/// ```gleam
/// pop_map([[], []], first)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// pop_map([], first)
/// // -> Error(Nil)
/// ```
///
pub fn pop_map(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Result(b, c),
) -> Result(#(b, List(a)), Nil) {
  do_pop_map(haystack, is_desired, [])
}

/// Given a list of 2-element tuples, finds the first tuple that has a given
/// key as the first element. This function will return the second element
/// of the found tuple and list with tuple removed.
///
/// If no tuple is found with the given key then `Error(Nil)` is returned.
///
/// ## Examples
///
/// ```gleam
/// key_pop([#("a", 0), #("b", 1)], "a")
/// // -> Ok(#(0, [#("b", 1)]))
/// ```
///
/// ```gleam
/// key_pop([#("a", 0), #("b", 1)], "b")
/// // -> Ok(#(1, [#("a", 0)]))
/// ```
///
/// ```gleam
/// key_pop([#("a", 0), #("b", 1)], "c")
/// // -> Error(Nil)
/// ```
///
pub fn key_pop(list: List(#(k, v)), key: k) -> Result(#(v, List(#(k, v))), Nil) {
  pop_map(list, fn(entry) {
    let #(k, v) = entry
    case k {
      k if k == key -> Ok(v)
      _ -> Error(Nil)
    }
  })
}

/// Given a list of 2-element tuples, inserts a key and value into the list.
///
/// If there was already a tuple with the key then it is replaced, otherwise it
/// is added to the end of the list.
///
/// ## Examples
///
/// ```gleam
/// key_set([#(5, 0), #(4, 1)], 4, 100)
/// // -> [#(5, 0), #(4, 100)]
/// ```
///
/// ```gleam
/// key_set([#(5, 0), #(4, 1)], 1, 100)
/// // -> [#(5, 0), #(4, 1), #(1, 100)]
/// ```
///
pub fn key_set(list: List(#(k, v)), key: k, value: v) -> List(#(k, v)) {
  case list {
    [] -> [#(key, value)]
    [#(k, _), ..rest] if k == key -> [#(key, value), ..rest]
    [first, ..rest] -> [first, ..key_set(rest, key, value)]
  }
}

/// Calls a function for each element in a list, discarding the return value.
///
/// Useful for calling a side effect for every item of a list.
///
/// ```gleam
/// import gleam/io
///
/// each(["1", "2", "3"], io.println)
/// // -> Nil
/// // 1
/// // 2
/// // 3
/// ```
///
pub fn each(list: List(a), f: fn(a) -> b) -> Nil {
  case list {
    [] -> Nil
    [first, ..rest] -> {
      f(first)
      each(rest, f)
    }
  }
}

/// Calls a `Result` returning function for each element in a list, discarding
/// the return value. If the function returns `Error` then the iteration is
/// stopped and the error is returned.
///
/// Useful for calling a side effect for every item of a list.
///
/// ## Examples
///
/// ```gleam
/// try_each(
///   over: [1, 2, 3],
///   with: function_that_might_fail,
/// )
/// // -> Ok(Nil)
/// ```
///
pub fn try_each(
  over list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(Nil, e) {
  case list {
    [] -> Ok(Nil)
    [first, ..rest] ->
      case fun(first) {
        Ok(_) -> try_each(over: rest, with: fun)
        Error(e) -> Error(e)
      }
  }
}

fn do_partition(list, categorise, trues, falses) {
  case list {
    [] -> #(reverse(trues), reverse(falses))
    [first, ..rest] ->
      case categorise(first) {
        True -> do_partition(rest, categorise, [first, ..trues], falses)
        False -> do_partition(rest, categorise, trues, [first, ..falses])
      }
  }
}

/// Partitions a list into a tuple/pair of lists
/// by a given categorisation function.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// [1, 2, 3, 4, 5] |> partition(int.is_odd)
/// // -> #([1, 3, 5], [2, 4])
/// ```
///
pub fn partition(
  list: List(a),
  with categorise: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  do_partition(list, categorise, [], [])
}

/// Returns all the permutations of a list.
///
/// ## Examples
///
/// ```gleam
/// permutations([1, 2])
/// // -> [[1, 2], [2, 1]]
/// ```
///
pub fn permutations(list: List(a)) -> List(List(a)) {
  case list {
    [] -> [[]]
    _ ->
      index_map(list, fn(i, i_idx) {
        index_fold(list, [], fn(acc, j, j_idx) {
          case i_idx == j_idx {
            True -> acc
            False -> [j, ..acc]
          }
        })
        |> reverse
        |> permutations
        |> map(fn(permutation) { [i, ..permutation] })
      })
      |> flatten
  }
}

fn do_window(acc: List(List(a)), list: List(a), n: Int) -> List(List(a)) {
  let window = take(list, n)

  case length(window) == n {
    True -> do_window([window, ..acc], drop(list, 1), n)
    False -> acc
  }
}

/// Returns a list of sliding windows.
///
/// ## Examples
///
/// ```gleam
/// window([1,2,3,4,5], 3)
/// // -> [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
/// ```
///
/// ```gleam
/// window([1, 2], 4)
/// // -> []
/// ```
///
pub fn window(list: List(a), by n: Int) -> List(List(a)) {
  case n <= 0 {
    True -> []
    False -> do_window([], list, n) |> reverse
  }
}

/// Returns a list of tuples containing two contiguous elements.
///
/// ## Examples
///
/// ```gleam
/// window_by_2([1,2,3,4])
/// // -> [#(1, 2), #(2, 3), #(3, 4)]
/// ```
///
/// ```gleam
/// window_by_2([1])
/// // -> []
/// ```
///
pub fn window_by_2(list: List(a)) -> List(#(a, a)) {
  zip(list, drop(list, 1))
}

/// Drops the first elements in a given list for which the predicate function returns `True`.
///
/// ## Examples
///
/// ```gleam
/// drop_while([1, 2, 3, 4], fn (x) { x < 3 })
/// // -> [3, 4]
/// ```
///
pub fn drop_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  case list {
    [] -> []
    [first, ..rest] ->
      case predicate(first) {
        True -> drop_while(rest, predicate)
        False -> [first, ..rest]
      }
  }
}

fn do_take_while(
  list: List(a),
  predicate: fn(a) -> Bool,
  acc: List(a),
) -> List(a) {
  case list {
    [] -> reverse(acc)
    [first, ..rest] ->
      case predicate(first) {
        True -> do_take_while(rest, predicate, [first, ..acc])
        False -> reverse(acc)
      }
  }
}

/// Takes the first elements in a given list for which the predicate function returns `True`.
///
/// ## Examples
///
/// ```gleam
/// take_while([1, 2, 3, 2, 4], fn (x) { x < 3 })
/// // -> [1, 2]
/// ```
///
pub fn take_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  do_take_while(list, predicate, [])
}

fn do_chunk(
  list: List(a),
  f: fn(a) -> k,
  previous_key: k,
  current_chunk: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [first, ..rest] -> {
      let key = f(first)
      case key == previous_key {
        False -> {
          let new_acc = [reverse(current_chunk), ..acc]
          do_chunk(rest, f, key, [first], new_acc)
        }
        _true -> do_chunk(rest, f, key, [first, ..current_chunk], acc)
      }
    }
    _empty -> reverse([reverse(current_chunk), ..acc])
  }
}

/// Returns a list of chunks in which
/// the return value of calling `f` on each element is the same.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 2, 3, 4, 4, 6, 7, 7] |> chunk(by: fn(n) { n % 2 })
/// // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
/// ```
///
pub fn chunk(in list: List(a), by f: fn(a) -> k) -> List(List(a)) {
  case list {
    [] -> []
    [first, ..rest] -> do_chunk(rest, f, f(first), [first], [])
  }
}

fn do_sized_chunk(
  list: List(a),
  count: Int,
  left: Int,
  current_chunk: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [] ->
      case current_chunk {
        [] -> reverse(acc)
        remaining -> reverse([reverse(remaining), ..acc])
      }
    [first, ..rest] -> {
      let chunk = [first, ..current_chunk]
      case left > 1 {
        False -> do_sized_chunk(rest, count, count, [], [reverse(chunk), ..acc])
        True -> do_sized_chunk(rest, count, left - 1, chunk, acc)
      }
    }
  }
}

/// Returns a list of chunks containing `count` elements each.
///
/// If the last chunk does not have `count` elements, it is instead
/// a partial chunk, with less than `count` elements.
///
/// For any `count` less than 1 this function behaves as if it was set to 1.
///
/// ## Examples
///
/// ```gleam
/// [1, 2, 3, 4, 5, 6] |> sized_chunk(into: 2)
/// // -> [[1, 2], [3, 4], [5, 6]]
/// ```
///
/// ```gleam
/// [1, 2, 3, 4, 5, 6, 7, 8] |> sized_chunk(into: 3)
/// // -> [[1, 2, 3], [4, 5, 6], [7, 8]]
/// ```
///
pub fn sized_chunk(in list: List(a), into count: Int) -> List(List(a)) {
  do_sized_chunk(list, count, count, [], [])
}

/// This function acts similar to fold, but does not take an initial state.
/// Instead, it starts from the first element in the list
/// and combines it with each subsequent element in turn using the given
/// function. The function is called as `fun(accumulator, current_element)`.
///
/// Returns `Ok` to indicate a successful run, and `Error` if called on an
/// empty list.
///
/// ## Examples
///
/// ```gleam
/// [] |> reduce(fn(acc, x) { acc + x })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// [1, 2, 3, 4, 5] |> reduce(fn(acc, x) { acc + x })
/// // -> Ok(15)
/// ```
///
pub fn reduce(over list: List(a), with fun: fn(a, a) -> a) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] -> Ok(fold(rest, first, fun))
  }
}

fn do_scan(
  list: List(a),
  accumulator: acc,
  accumulated: List(acc),
  fun: fn(acc, a) -> acc,
) -> List(acc) {
  case list {
    [] -> reverse(accumulated)
    [first, ..rest] -> {
      let next = fun(accumulator, first)
      do_scan(rest, next, [next, ..accumulated], fun)
    }
  }
}

/// Similar to `fold`, but yields the state of the accumulator at each stage.
///
/// ## Examples
///
/// ```gleam
/// scan(over: [1, 2, 3], from: 100, with: fn(acc, i) { acc + i })
/// // -> [101, 103, 106]
/// ```
///
pub fn scan(
  over list: List(a),
  from initial: acc,
  with fun: fn(acc, a) -> acc,
) -> List(acc) {
  do_scan(list, initial, [], fun)
}

/// Returns the last element in the given list.
///
/// Returns `Error(Nil)` if the list is empty.
///
/// This function runs in linear time.
/// For a collection oriented around performant access at either end,
/// see `gleam/queue.Queue`.
///
/// ## Examples
///
/// ```gleam
/// last([])
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// last([1, 2, 3, 4, 5])
/// // -> Ok(5)
/// ```
///
pub fn last(list: List(a)) -> Result(a, Nil) {
  list
  |> reduce(fn(_, elem) { elem })
}

/// Return unique combinations of elements in the list.
///
/// ## Examples
///
/// ```gleam
/// combinations([1, 2, 3], 2)
/// // -> [[1, 2], [1, 3], [2, 3]]
/// ```
///
/// ```gleam
/// combinations([1, 2, 3, 4], 3)
/// // -> [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
/// ```
///
pub fn combinations(items: List(a), by n: Int) -> List(List(a)) {
  case n {
    0 -> [[]]
    _ ->
      case items {
        [] -> []
        [first, ..rest] -> {
          let first_combinations =
            map(combinations(rest, n - 1), with: fn(com) { [first, ..com] })
            |> reverse
          fold(first_combinations, combinations(rest, n), fn(acc, c) {
            [c, ..acc]
          })
        }
      }
  }
}

fn do_combination_pairs(items: List(a)) -> List(List(#(a, a))) {
  case items {
    [] -> []
    [first, ..rest] -> {
      let first_combinations = map(rest, with: fn(other) { #(first, other) })
      [first_combinations, ..do_combination_pairs(rest)]
    }
  }
}

/// Return unique pair combinations of elements in the list
///
/// ## Examples
///
/// ```gleam
/// combination_pairs([1, 2, 3])
/// // -> [#(1, 2), #(1, 3), #(2, 3)]
/// ```
///
pub fn combination_pairs(items: List(a)) -> List(#(a, a)) {
  do_combination_pairs(items)
  |> flatten
}

/// Make a list alternating the elements from the given lists
///
/// ## Examples
///
/// ```gleam
/// interleave([[1, 2], [101, 102], [201, 202]])
/// // -> [1, 101, 201, 2, 102, 202]
/// ```
///
pub fn interleave(list: List(List(a))) -> List(a) {
  transpose(list)
  |> flatten
}

/// Transpose rows and columns of the list of lists.
///
/// Notice: This function is not tail recursive,
/// and thus may exceed stack size if called,
/// with large lists (on target JavaScript).
///
/// ## Examples
///
/// ```gleam
/// transpose([[1, 2, 3], [101, 102, 103]])
/// // -> [[1, 101], [2, 102], [3, 103]]
/// ```
///
pub fn transpose(list_of_list: List(List(a))) -> List(List(a)) {
  let take_first = fn(list) {
    case list {
      [] -> []
      [f] -> [f]
      [f, ..] -> [f]
    }
  }

  case list_of_list {
    [] -> []
    [[], ..rest] -> transpose(rest)
    rows -> {
      let firsts =
        rows
        |> map(take_first)
        |> flatten
      let rest = transpose(map(rows, drop(_, 1)))
      [firsts, ..rest]
    }
  }
}

fn do_shuffle_pair_unwrap(list: List(#(Float, a)), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [elem_pair, ..enumerable] ->
      do_shuffle_pair_unwrap(enumerable, [elem_pair.1, ..acc])
  }
}

fn do_shuffle_by_pair_indexes(
  list_of_pairs: List(#(Float, a)),
) -> List(#(Float, a)) {
  sort(list_of_pairs, fn(a_pair: #(Float, a), b_pair: #(Float, a)) -> Order {
    float.compare(a_pair.0, b_pair.0)
  })
}

/// Takes a list, randomly sorts all items and returns the shuffled list.
///
/// This function uses `float.random` to decide the order of the elements.
///
/// ## Example
///
/// ```gleam
/// range(1, 10) |> shuffle()
/// // -> [1, 6, 9, 10, 3, 8, 4, 2, 7, 5]
/// ```
///
pub fn shuffle(list: List(a)) -> List(a) {
  list
  |> fold(from: [], with: fn(acc, a) { [#(float.random(), a), ..acc] })
  |> do_shuffle_by_pair_indexes()
  |> do_shuffle_pair_unwrap([])
}
