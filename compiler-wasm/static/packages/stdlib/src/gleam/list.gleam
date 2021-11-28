//// Lists are an ordered sequence of elements and are one of the most common
//// data types in Gleam.
////
//// New elements can be added and removed from the front of a list in
//// constant time, while adding and removing from the end requires traversing
//// the copying the whole list, so keep this in mind when designing your
//// programs.
////
//// There is a dedicated syntax for prefixing to a list:
////
////    let new_list = [1, 2, ..existing_list]
////
//// And a matching syntax for getting the first elements of a list:
////
////    case list {
////      [first_element, ..rest] -> first_element
////      _ -> "this pattern matches when the list is empty"
////    }
////

import gleam/int
import gleam/pair
import gleam/order.{Order}

/// An error value returned by the `strict_zip` function.
///
pub type LengthMismatch {
  LengthMismatch
}

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
///    > length([])
///    0
///
///    > length([1])
///    1
///
///    > length([1, 2])
///    2
///
pub fn length(of list: List(a)) -> Int {
  do_length(list)
}

if erlang {
  external fn do_length(List(a)) -> Int =
    "erlang" "length"
}

if javascript {
  fn do_length(list: List(a)) -> Int {
    do_length_acc(list, 0)
  }

  fn do_length_acc(list: List(a), count: Int) -> Int {
    case list {
      [_, ..list] -> do_length_acc(list, count + 1)
      _ -> count
    }
  }
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
///    > reverse([])
///    []
///
///    > reverse([1])
///    [1]
///
///    > reverse([1, 2])
///    [2, 1]
///
pub fn reverse(xs: List(a)) -> List(a) {
  do_reverse(xs)
}

if erlang {
  external fn do_reverse(List(a)) -> List(a) =
    "lists" "reverse"
}

if javascript {
  fn do_reverse(list) {
    do_reverse_acc(list, [])
  }

  fn do_reverse_acc(remaining, accumulator) {
    case remaining {
      [] -> accumulator
      [item, ..rest] -> do_reverse_acc(rest, [item, ..accumulator])
    }
  }
}

/// Determines whether or not the list is empty.
///
/// This function runs in constant time.
///
/// ## Examples
///
///    > is_empty([])
///    True
///
///    > is_empty([1])
///    False
///
///    > is_empty([1, 1])
///    False
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
///    > [] |> contains(any: 0)
///    False
///
///    > [0] |> contains(any: 0)
///    True
///
///    > [1] |> contains(any: 0)
///    False
///
///    > [1, 1] |> contains(any: 0)
///    False
///
///    > [1, 0] |> contains(any: 0)
///    True
///
pub fn contains(list: List(a), any elem: a) -> Bool {
  case list {
    [] -> False
    [head, ..rest] -> head == elem || contains(rest, elem)
  }
}

/// Gets the first element from the start of the list, if there is one.
///
/// ## Examples
///
///    > first([])
///    Error(Nil)
///
///    > first([0])
///    Ok(0)
///
///    > first([1, 2])
///    Ok(1)
///
pub fn first(list: List(a)) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [x, .._] -> Ok(x)
  }
}

/// Gets the list minus the first element. If the list is empty `Error(Nil)` is
/// returned.
///
/// This function runs in constant time and does not make a copy of the list.
///
/// ## Examples
///
///    > rest([])
///    Error(Nil)
///
///    > rest([0])
///    Ok([])
///
///    > rest([1, 2])
///    Ok([2])
///
pub fn rest(list: List(a)) -> Result(List(a), Nil) {
  case list {
    [] -> Error(Nil)
    [_, ..xs] -> Ok(xs)
  }
}

fn do_filter(list: List(a), fun: fn(a) -> Bool, acc: List(a)) -> List(a) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> {
      let new_acc = case fun(x) {
        True -> [x, ..acc]
        False -> acc
      }
      do_filter(xs, fun, new_acc)
    }
  }
}

/// Returns a new list containing only the elements from the first list for
/// which the given functions returns `True`.
///
/// ## Examples
///
///    > filter([2, 4, 6, 1], fn(x) { x > 2 })
///    [4, 6]
///
///    > filter([2, 4, 6, 1], fn(x) { x > 6 })
///    []
///
pub fn filter(list: List(a), for predicate: fn(a) -> Bool) -> List(a) {
  do_filter(list, predicate, [])
}

fn do_filter_map(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> {
      let new_acc = case fun(x) {
        Ok(x) -> [x, ..acc]
        Error(_) -> acc
      }
      do_filter_map(xs, fun, new_acc)
    }
  }
}

/// Returns a new list containing only the elements from the first list for
/// which the given functions returns `Ok(_)`.
///
/// ## Examples
///
///    > filter_map([2, 4, 6, 1], Error)
///    []
///
///    > filter_map([2, 4, 6, 1], fn(x) { Ok(x + 1) })
///    [3, 5, 7, 2]
///
pub fn filter_map(list: List(a), with fun: fn(a) -> Result(b, e)) -> List(b) {
  do_filter_map(list, fun, [])
}

fn do_map(list: List(a), fun: fn(a) -> b, acc: List(b)) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> do_map(xs, fun, [fun(x), ..acc])
  }
}

/// Returns a new list containing only the elements of the first list after the
/// function has been applied to each one.
///
/// ## Examples
///
///    > map([2, 4, 6], fn(x) { x * 2 })
///    [4, 8, 12]
///
pub fn map(list: List(a), with fun: fn(a) -> b) -> List(b) {
  do_map(list, fun, [])
}

/// Similar to map but also lets you pass around an accumulated value.
///
/// ## Examples
///
/// ```
/// > map_fold(
///     over: [1, 2, 3],
///     from: 100,
///     with: fn(i, memo) { #(i * 2, memo + i) }
///  )
///  #([2, 4, 6], 106)
/// ```
///
pub fn map_fold(
  over list: List(a),
  from acc: acc,
  with fun: fn(acc, a) -> #(acc, b),
) -> #(acc, List(b)) {
  fold(
    over: list,
    from: #(acc, []),
    with: fn(acc, item) {
      let #(current_acc, items) = acc
      let #(next_acc, next_item) = fun(current_acc, item)
      #(next_acc, [next_item, ..items])
    },
  )
  |> pair.map_second(reverse)
}

fn do_index_map(
  list: List(a),
  fun: fn(Int, a) -> b,
  index: Int,
  acc: List(b),
) -> List(b) {
  case list {
    [] -> reverse(acc)
    [x, ..xs] -> {
      let acc = [fun(index, x), ..acc]
      do_index_map(xs, fun, index + 1, acc)
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
///    > index_map(["a", "b"], fn(i, x) { #(i, x) })
///    [#(0, "a"), #(1, "b")]
///
pub fn index_map(list: List(a), with fun: fn(Int, a) -> b) -> List(b) {
  do_index_map(list, fun, 0, [])
}

fn do_try_map(
  list: List(a),
  fun: fn(a) -> Result(b, e),
  acc: List(b),
) -> Result(List(b), e) {
  case list {
    [] -> Ok(reverse(acc))
    [x, ..xs] ->
      case fun(x) {
        Ok(y) -> do_try_map(xs, fun, [y, ..acc])
        Error(error) -> Error(error)
      }
  }
}

/// Takes a function that returns a Result applies it to each element in a
/// given list in tern.
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
///    > try_map([1, 2, 3], fn(x) { Ok(x + 2) })
///    Ok([3, 4, 5])
///
///    > try_map([1, 2, 3], fn(_) { Error(0) })
///    Error(0)
///
///    > try_map([[1], [2, 3]], head)
///    Ok([1, 2])
///
///    > try_map([[1], [], [2]], head)
///    Error(Nil)
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
///    > drop([1, 2, 3, 4], 2)
///    [3, 4]
///
///    > drop([1, 2, 3, 4], 9)
///    []
///
pub fn drop(from list: List(a), up_to n: Int) -> List(a) {
  case n <= 0 {
    True -> list
    False ->
      case list {
        [] -> []
        [_, ..xs] -> drop(xs, n - 1)
      }
  }
}

fn do_take(list: List(a), n: Int, acc: List(a)) -> List(a) {
  case n <= 0 {
    True -> reverse(acc)
    False ->
      case list {
        [] -> reverse(acc)
        [x, ..xs] -> do_take(xs, n - 1, [x, ..acc])
      }
  }
}

/// Returns a list containing the first given number of elements from the given
/// list.
///
/// If the element has less than the number of elements then the full list is
/// returned.
///
/// This function runs in linear time but does not copy the list.
///
/// ## Examples
///
///    > take([1, 2, 3, 4], 2)
///    [1, 2]
///
///    > take([1, 2, 3, 4], 9)
///    [1, 2, 3, 4]
///
pub fn take(from list: List(a), up_to n: Int) -> List(a) {
  do_take(list, n, [])
}

/// Returns a new empty list.
///
/// ## Examples
///
///    > new()
///    []
///
pub fn new() -> List(a) {
  []
}

/// Joins one list onto the end of another.
///
/// This function runs in linear time, and it traverses and copies the first
/// list.
///
/// ## Examples
///
///    > append([1, 2], [3])
///    [1, 2, 3]
///
pub fn append(first: List(a), second: List(a)) -> List(a) {
  do_append(first, second)
}

if erlang {
  external fn do_append(List(a), List(a)) -> List(a) =
    "lists" "append"
}

if javascript {
  fn do_append(first: List(a), second: List(a)) -> List(a) {
    do_append_acc(reverse(first), second)
  }

  fn do_append_acc(first: List(a), second: List(a)) -> List(a) {
    case first {
      [] -> second
      [item, ..rest] -> do_append_acc(rest, [item, ..second])
    }
  }
}

fn do_flatten(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> acc
    [l, ..rest] -> do_flatten(rest, append(acc, l))
  }
}

/// Flattens a list of lists into a single list.
///
/// This function runs in linear time, and it traverses and copies all the
/// inner lists.
///
/// ## Examples
///
///    > flatten([[1], [2, 3], []])
///    [1, 2, 3]
///
pub fn flatten(lists: List(List(a))) -> List(a) {
  do_flatten(lists, [])
}

/// Map and flatten the result
///
/// ## Examples
///
/// ```
/// > flat_map([2, 4, 6], fn(x) { [x, x + 1] })
/// [2, 3, 4, 5, 6, 7]
/// ```
///
pub fn flat_map(over list: List(a), with fun: fn(a) -> List(b)) -> List(b) {
  map(list, fun)
  |> flatten
}

/// Reduces a list of elements into a single value by calling a given function
/// on each element, going from left to right.
///
/// `fold([1, 2, 3], 0, add)` is the equivalent of `add(3, add(2, add(1, 0)))`.
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
/// `add(1, add(2, add(3, 0)))`.
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
/// ```
/// ["a", "b", "c"]
/// |> list.index_fold([], fn(index, item, acc) { ... })
/// ```
///
pub fn index_fold(
  over over: List(a),
  from initial: acc,
  with fun: fn(acc, a, Int) -> acc,
) -> acc {
  do_index_fold(over, initial, fun, 0)
}

/// A variant of fold that might fail.
///
/// The folding function should return `Result(accumulator, error)
/// If the returned value is `Ok(accumulator)` try_fold will try the next value in the list.
/// If the returned value is `Error(error)` try_fold will stop and return that error.
///
/// ## Examples
///
/// ```
/// [1, 2, 3, 4]
/// |> try_fold(0, fn(i, acc) {
///   case i < 3 {
///     True -> Ok(acc + i)
///     False -> Error(Nil)
///   }
/// })
/// ```
///
pub fn try_fold(
  over collection: List(a),
  from accumulator: acc,
  with fun: fn(acc, a) -> Result(acc, e),
) -> Result(acc, e) {
  case collection {
    [] -> Ok(accumulator)
    [first, ..rest] -> {
      try accumulator = fun(accumulator, first)
      try_fold(rest, accumulator, fun)
    }
  }
}

pub type ContinueOrStop(a) {
  Continue(a)
  Stop(a)
}

/// A variant of fold that allows to stop folding earlier.
///
/// The folding function should return `ContinueOrStop(accumulator)
/// If the returned value is `Continue(accumulator)` fold_until will try the next value in the list.
/// If the returned value is `Stop(accumulator)` fold_until will stop and return that accumulator.
///
/// ## Examples
///
/// ```
/// [1, 2, 3, 4]
/// |> fold_until(0, fn(i, acc) {
///   case i < 3 {
///     True -> Continue(acc + i)
///     False -> Stop(acc)
///   }
/// })
/// ```
///
pub fn fold_until(
  over collection: List(a),
  from accumulator: acc,
  with fun: fn(acc, a) -> ContinueOrStop(acc),
) -> acc {
  case collection {
    [] -> accumulator
    [first, ..rest] ->
      case fun(accumulator, first) {
        Continue(next_accumulator) -> fold_until(rest, next_accumulator, fun)
        Stop(b) -> b
      }
  }
}

/// Finds the first element in a given list for which the given function returns
/// True.
///
/// Returns `Error(Nil)` if no the function does not return True for any of the
/// elements.
///
/// ## Examples
///
///    > find([1, 2, 3], fn(x) { x > 2 })
///    Ok(3)
///
///    > find([1, 2, 3], fn(x) { x > 4 })
///    Error(Nil)
///
///    > find([], fn(_) { True })
///    Error(Nil)
///
pub fn find(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case is_desired(x) {
        True -> Ok(x)
        _ -> find(in: rest, one_that: is_desired)
      }
  }
}

/// Finds the first element in a given list for which the given function returns
/// `Ok(new_value)` and return the new value for that element.
///
/// Returns `Error(Nil)` if no the function does not return Ok for any of the
/// elements.
///
/// ## Examples
///
///    > find_map([[], [2], [3]], head)
///    Ok(2)
///
///    > find_map([[], []], head)
///    Error(Nil)
///
///    > find_map([], head)
///    Error(Nil)
///
pub fn find_map(
  in haystack: List(a),
  with fun: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case fun(x) {
        Ok(x) -> Ok(x)
        _ -> find_map(in: rest, with: fun)
      }
  }
}

/// Returns True if the given function returns True for all the elements in
/// the given list. If the function returns False for any of the elements it
/// immediately returns False without checking the rest of the list.
///
/// ## Examples
///
///    > all([], fn(x) { x > 3 })
///    True
///
///    > all([4, 5], fn(x) { x > 3 })
///    True
///
///    > all([4, 3], fn(x) { x > 3 })
///    False
///
pub fn all(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> True
    [x, ..rest] -> predicate(x) && all(rest, predicate)
  }
}

/// Returns True if the given function returns True for any the elements in
/// the given list. If the function returns True for any of the elements it
/// immediately returns True without checking the rest of the list.
///
/// ## Examples
///
///    > any([], fn(x) { x > 3 })
///    False
///
///    > any([4, 5], fn(x) { x > 3 })
///    True
///
///    > any([4, 3], fn(x) { x > 4 })
///    False
///
///    > any([3, 4], fn(x) { x > 3 })
///    True
///
pub fn any(in list: List(a), satisfying predicate: fn(a) -> Bool) -> Bool {
  case list {
    [] -> False
    [x, ..rest] -> predicate(x) || any(rest, predicate)
  }
}

fn do_zip(xs: List(a), ys: List(b), acc: List(#(a, b))) -> List(#(a, b)) {
  case xs, ys {
    [x, ..xs], [y, ..ys] -> do_zip(xs, ys, [#(x, y), ..acc])
    _, _ -> reverse(acc)
  }
}

/// Takes two lists and returns a single list of 2 item tuples.
///
/// If one of the lists is longer than the other the remaining elements from
/// the longer list are not used.
///
/// ## Examples
///
///    > zip([], [])
///    []
///
///    > zip([1, 2], [3])
///    [#(1, 3)]
///
///    > zip([1], [3, 4])
///    [#(1, 3)]
///
///    > zip([1, 2], [3, 4])
///    [#(1, 3), #(2, 4)]
///
pub fn zip(xs: List(a), ys: List(b)) -> List(#(a, b)) {
  do_zip(xs, ys, [])
}

/// Takes two lists and returns a single list of 2 item tuples.
///
/// If one of the lists is longer than the other an Error is returned.
///
/// ## Examples
///
///    > strict_zip([], [])
///    Ok([])
///
///    > strict_zip([1, 2], [3])
///    Error(LengthMismatch)
///
///    > strict_zip([1], [3, 4])
///    Error(LengthMismatch)
///
///    > strict_zip([1, 2], [3, 4])
///    Ok([#(1, 3), #(2, 4)])
///
pub fn strict_zip(
  l1: List(a),
  l2: List(b),
) -> Result(List(#(a, b)), LengthMismatch) {
  case length(of: l1) == length(of: l2) {
    True -> Ok(zip(l1, l2))
    False -> Error(LengthMismatch)
  }
}

fn do_unzip(input, xs, ys) {
  case input {
    [] -> #(reverse(xs), reverse(ys))
    [#(x, y), ..rest] -> do_unzip(rest, [x, ..xs], [y, ..ys])
  }
}

/// Takes a single list of 2 item tuples and returns two lists.
///
/// ## Examples
///
///    > unzip([#(1, 2), #(3, 4)])
///    #([1, 3], [2, 4])
///
///    > unzip([])
///    #([], [])
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
///    > intersperse([1, 1, 1], 2)
///    [1, 2, 1, 2, 1]
///
///    > intersperse([], 2)
///    []
///
pub fn intersperse(list: List(a), with elem: a) -> List(a) {
  case list {
    [] | [_] -> list
    [x, ..rest] -> do_intersperse(rest, elem, [x])
  }
}

/// Returns the element in the Nth position in the list, with 0 being the first
/// position.
///
/// Error(Nil) is returned if the list is not long enough for the given index.
///
/// For any `index` less than 0 this function behaves as if it was set to 0.
///
/// ## Examples
///
///    > at([1, 2, 3], 1)
///    Ok(2)
///
///    > at([1, 2, 3], 5)
///    Error(Nil)
///
pub fn at(in list: List(a), get index: Int) -> Result(a, Nil) {
  list
  |> drop(index)
  |> first
}

/// Removes any duplicate elements from a given list.
///
/// This function returns in log-linear time (n log n).
///
/// ## Examples
///
///    > unique([1, 1, 1, 4, 7, 3, 3, 4])
///    [1, 4, 7, 3]
///
pub fn unique(list: List(a)) -> List(a) {
  case list {
    [] -> []
    [x, ..rest] -> [x, ..unique(filter(rest, fn(y) { y != x }))]
  }
}

fn merge_sort(a: List(a), b: List(a), compare: fn(a, a) -> Order) -> List(a) {
  case a, b {
    [], _ -> b
    _, [] -> a
    [ax, ..ar], [bx, ..br] ->
      case compare(ax, bx) {
        order.Lt -> [ax, ..merge_sort(ar, b, compare)]
        _ -> [bx, ..merge_sort(a, br, compare)]
      }
  }
}

fn do_sort(
  list: List(a),
  compare: fn(a, a) -> Order,
  list_length: Int,
) -> List(a) {
  case list_length < 2 {
    True -> list
    False -> {
      let split_length = list_length / 2
      let a_list = take(list, split_length)
      let b_list = drop(list, split_length)
      merge_sort(
        do_sort(a_list, compare, split_length),
        do_sort(b_list, compare, list_length - split_length),
        compare,
      )
    }
  }
}

/// Sorts from smallest to largest based upon the ordering specified by a given
/// function.
///
/// ## Examples
///
///    > import gleam/int
///    > list.sort([4, 3, 6, 5, 4, 1, 2], by: int.compare)
///    [1, 2, 3, 4, 4, 5, 6]
///
pub fn sort(list: List(a), by compare: fn(a, a) -> Order) -> List(a) {
  do_sort(list, compare, length(list))
}

/// Creates a list of ints ranging from a given start and finish.
///
/// ## Examples
///
///    > range(0, 0)
///    []
///
///    > range(0, 5)
///    [0, 1, 2, 3, 4]
///
///    > range(1, -5)
///    [1, 0, -1, -2, -3, -4]
///
pub fn range(from start: Int, to stop: Int) -> List(Int) {
  case int.compare(start, stop) {
    order.Eq -> []
    order.Gt -> [start, ..range(start - 1, stop)]
    order.Lt -> [start, ..range(start + 1, stop)]
  }
}

fn do_repeat(a: a, times: Int, acc: List(a)) -> List(a) {
  case times <= 0 {
    True -> acc
    False -> do_repeat(a, times - 1, [a, ..acc])
  }
}

/// Builds a list of a given value a given number of times.
///
/// ## Examples
///
///    > repeat("a", times: 0)
///    []
///
///    > repeat("a", times: 5)
///    ["a", "a", "a", "a", "a"]
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
        [x, ..xs] -> do_split(xs, n - 1, [x, ..taken])
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
///    > split([6, 7, 8, 9], 0)
///    #([], [6, 7, 8, 9])
///
///    > split([6, 7, 8, 9], 2)
///    #([6, 7], [8, 9])
///
///    > split([6, 7, 8, 9], 4)
///    #([6, 7, 8, 9], [])
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
    [x, ..xs] ->
      case f(x) {
        False -> #(reverse(acc), list)
        _ -> do_split_while(xs, f, [x, ..acc])
      }
  }
}

/// Splits a list in two before the first element that a given function returns
/// False for.
///
/// If the function returns True for all elements the first list will be the
/// input list, and the second list will be empty.
///
/// ## Examples
///
///    > split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })
///    #([1, 2, 3], [4, 5])
///
///    > split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })
///    #([1, 2, 3, 4, 5], [])
///
pub fn split_while(
  list list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  do_split_while(list, predicate, [])
}

/// Given a list of 2 element tuples, finds the first tuple that has a given
/// key as the first element and returns the second element.
///
/// If no tuple is found with the given key then `Error(Nil)` is returned.
///
/// This function may be useful for interacting with Erlang code where lists of
/// tuples are common.
///
/// ## Examples
///
///    > key_find([#("a", 0), #("b", 1)], "a")
///    Ok(0)
///
///    > key_find([#("a", 0), #("b", 1)], "b")
///    Ok(1)
///
///    > key_find([#("a", 0), #("b", 1)], "c")
///    Error(Nil)
///
pub fn key_find(
  in keyword_list: List(#(k, v)),
  find desired_key: k,
) -> Result(v, Nil) {
  find_map(
    keyword_list,
    fn(keyword) {
      let #(key, value) = keyword
      case key == desired_key {
        True -> Ok(value)
        False -> Error(Nil)
      }
    },
  )
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

/// Removes the first element in a given list for which the predicate funtion returns `True`.
///
/// Returns `Error(Nil)` if no the function does not return True for any of the
/// elements.
///
/// ## Examples
///
///    > pop([1, 2, 3], fn(x) { x > 2 })
///    Ok(#(3, [1, 2]))
///
///    > pop([1, 2, 3], fn(x) { x > 4 })
///    Error(Nil)
///
///    > pop([], fn(_) { True })
///    Error(Nil)
///
pub fn pop(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(#(a, List(a)), Nil) {
  do_pop(haystack, is_desired, [])
}

fn do_pop_map(haystack, mapper, checked) {
  case haystack {
    [] -> Error(Nil)
    [x, ..rest] ->
      case mapper(x) {
        Ok(y) -> Ok(#(y, append(reverse(checked), rest)))
        Error(_) -> do_pop_map(rest, mapper, [x, ..checked])
      }
  }
}

/// Removes the first element in a given list for which the given function returns
/// `Ok(new_value)` and return the new value as well as list with the value removed.
///
/// Returns `Error(Nil)` if no the function does not return Ok for any of the
/// elements.
///
/// ## Examples
///
///    > pop_map([[], [2], [3]], head)
///    Ok(#(2, [[], [3]]))
///
///    > pop_map([[], []], head)
///    Error(Nil)
///
///    > pop_map([], head)
///    Error(Nil)
///
pub fn pop_map(
  in haystack: List(a),
  one_that is_desired: fn(a) -> Result(b, c),
) -> Result(#(b, List(a)), Nil) {
  do_pop_map(haystack, is_desired, [])
}

/// Given a list of 2 element tuples, finds the first tuple that has a given
/// key as the first element. This function will return the second element
/// of the found tuple and list with tuple removed.
///
/// If no tuple is found with the given key then `Error(Nil)` is returned.
///
/// ## Examples
///
///    > key_pop([#("a", 0), #("b", 1)], "a")
///    Ok(#(0, [#("b", 1)]))
///
///    > key_pop([#("a", 0), #("b", 1)], "b")
///    Ok(#(1, [#("a", 0)]))
///
///    > key_pop([#("a", 0), #("b", 1)], "c")
///    Error(Nil)
///
pub fn key_pop(
  haystack: List(#(k, v)),
  key: k,
) -> Result(#(v, List(#(k, v))), Nil) {
  pop_map(
    haystack,
    fn(entry) {
      let #(k, v) = entry
      case k {
        k if k == key -> Ok(v)
        _ -> Error(Nil)
      }
    },
  )
}

/// Given a list of 2 element tuples, inserts a key and value into the list.
///
/// If there was already a tuple with the key then it is replaced, otherwise it
/// is added to the end of the list.
///
///
/// ## Examples
///
///    > key_set([#(5, 0), #(4, 1)], 4, 100)
///    [#(5, 0), #(4, 100)]
///
///    > key_set([#(5, 0), #(4, 1)], 1, 100)
///    [#(5, 0), #(4, 1), #(1, 100)]
///
pub fn key_set(list: List(#(a, b)), key: a, value: b) -> List(#(a, b)) {
  case list {
    [] -> [#(key, value)]
    [#(k, _), ..rest] if k == key -> [#(key, value), ..rest]
    [first, ..rest] -> [first, ..key_set(rest, key, value)]
  }
}

/// Calls a function for each element in a list, discarding the results.
///
pub fn each(list: List(a), f: fn(a) -> b) -> Nil {
  case list {
    [] -> Nil
    [x, ..xs] -> {
      f(x)
      each(xs, f)
    }
  }
}

fn do_partition(list, categorise, trues, falses) {
  case list {
    [] -> #(reverse(trues), reverse(falses))
    [x, ..xs] ->
      case categorise(x) {
        True -> do_partition(xs, categorise, [x, ..trues], falses)
        False -> do_partition(xs, categorise, trues, [x, ..falses])
      }
  }
}

pub fn partition(
  list: List(a),
  with categorise: fn(a) -> Bool,
) -> #(List(a), List(a)) {
  do_partition(list, categorise, [], [])
}

/// Returns all the permutations of a list
/// All values must be unique
///
/// ## Examples
///
///    > permutations([1, 2])
///    [[1, 2], [2, 1]]
///
pub fn permutations(l: List(a)) -> List(List(a)) {
  case l {
    [] -> [[]]
    _ ->
      map(
        l,
        fn(x) {
          filter(l, fn(y) { y != x })
          |> permutations
          |> map(append([x], _))
        },
      )
      |> flatten
  }
}

fn do_window(acc: List(List(a)), l: List(a), n: Int) -> List(List(a)) {
  let window = take(l, n)

  case length(window) == n {
    True -> do_window([window, ..acc], drop(l, 1), n)
    False -> acc
  }
}

/// Returns a list of sliding window
///
/// ## Examples
///
/// ```
/// > window([1,2,3,4,5], 3)
/// [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
///
/// > window([1, 2], 4)
/// []
/// ```
///
pub fn window(l: List(a), by n: Int) -> List(List(a)) {
  do_window([], l, n)
  |> reverse
}

/// Returns a list of tuples containing two contiguous elements
///
/// ## Examples
///
/// ```
/// > window_by_2([1,2,3,4])
/// [#(1, 2), #(2, 3), #(3, 4)]
///
/// > window_by_2([1])
/// []
/// ```
///
pub fn window_by_2(l: List(a)) -> List(#(a, a)) {
  zip(l, drop(l, 1))
}

/// Drops the first elements in a given list for which the predicate funtion returns `True`.
///
/// ## Examples
///
///    > drop_while([1, 2, 3, 4], fn (x) { x < 3 })
///    [3, 4]
///
pub fn drop_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  case list {
    [] -> []
    [x, ..xs] ->
      case predicate(x) {
        True -> drop_while(xs, predicate)
        False -> [x, ..xs]
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
    [head, ..tail] ->
      case predicate(head) {
        True -> do_take_while(tail, predicate, [head, ..acc])
        False -> reverse(acc)
      }
  }
}

/// Takes the first elements in a given list for which the predicate funtion returns `True`.
///
/// ## Examples
///
///    > take_while([1, 2, 3, 2, 4], fn (x) { x < 3 })
///    [1, 2]
///
pub fn take_while(
  in list: List(a),
  satisfying predicate: fn(a) -> Bool,
) -> List(a) {
  do_take_while(list, predicate, [])
}

fn do_chunk(
  list: List(a),
  f: fn(a) -> key,
  previous_key: key,
  current_chunk: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case list {
    [head, ..tail] -> {
      let key = f(head)
      case key == previous_key {
        False -> {
          let new_acc = [reverse(current_chunk), ..acc]
          do_chunk(tail, f, key, [head], new_acc)
        }
        _true -> do_chunk(tail, f, key, [head, ..current_chunk], acc)
      }
    }
    _empty -> reverse([reverse(current_chunk), ..acc])
  }
}

/// Returns a list of chunks in which
/// the result of calling `f` on each element is the same.
///
/// ## Examples
///
///    > [1, 2, 2, 3, 4, 4, 6, 7, 7] |> chunk(by: fn(n) { n % 2 })
///    [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
///
pub fn chunk(in list: List(a), by f: fn(a) -> key) -> List(List(a)) {
  case list {
    [] -> []
    [head, ..tail] -> do_chunk(tail, f, f(head), [head], [])
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
    [head, ..tail] -> {
      let chunk = [head, ..current_chunk]
      case left > 1 {
        False -> do_sized_chunk(tail, count, count, [], [reverse(chunk), ..acc])
        True -> do_sized_chunk(tail, count, left - 1, chunk, acc)
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
///    > [1, 2, 3, 4, 5, 6] |> sized_chunk(into: 2)
///    [[1, 2], [3, 4], [5, 6]]
///
///    > [1, 2, 3, 4, 5, 6, 7, 8] |> sized_chunk(into: 3)
///    [[1, 2, 3], [4, 5, 6], [7, 8]]
///
pub fn sized_chunk(in list: List(a), into count: Int) -> List(List(a)) {
  do_sized_chunk(list, count, count, [], [])
}

/// This function acts similar to fold, but does not take an initial state.
/// Instead, it starts from the first element in the list
/// and combines it with each subsequent element in turn using the given function.
/// The function is called as fun(current_element, accumulator).
///
/// Returns `Ok` to indicate a successful run, and `Error` if called on an empty list.
///
/// ## Examples
///
///    > [] |> reduce(fn(x, y) { x + y })
///    Error(Nil)
///
///    > [1, 2, 3, 4, 5] |> reduce(fn(x, y) { x + y })
///    Ok(15)
///
pub fn reduce(over list: List(a), with fun: fn(a, a) -> a) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [head, ..tail] -> Ok(fold(tail, head, fun))
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
    [x, ..xs] -> {
      let next = fun(accumulator, x)
      do_scan(xs, next, [next, ..accumulated], fun)
    }
  }
}

/// Similar to `fold`, but yields the state of the accumulator at each stage.
///
/// ## Examples
///
///    > scan(over: [1, 2, 3], from: 100, with: fn(i, acc) { acc + i })
///    [101, 103, 106]
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
///    > last([])
///    Error(Nil)
///
///    > last([1, 2, 3, 4, 5])
///    Ok(5)
///
pub fn last(list: List(a)) -> Result(a, Nil) {
  list
  |> reduce(fn(_, elem) { elem })
}

/// Return unique combinations of elements in the list
///
/// ## Examples
///
/// ```
/// > combinations([1, 2, 3], 2)
/// [[1, 2], [1, 3], [2, 3]]
///
///  > combinations([1, 2, 3, 4], 3)
///  [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
/// ```
///
pub fn combinations(items: List(a), by n: Int) -> List(List(a)) {
  case n {
    0 -> [[]]
    _ ->
      case items {
        [] -> []
        [x, ..xs] -> {
          let first_combinations =
            map(combinations(xs, n - 1), with: fn(com) { [x, ..com] })
            |> reverse
          fold(
            first_combinations,
            combinations(xs, n),
            fn(acc, c) { [c, ..acc] },
          )
        }
      }
  }
}

fn do_combination_pairs(items: List(a)) -> List(List(#(a, a))) {
  case items {
    [] -> []
    [x, ..xs] -> {
      let first_combinations = map(xs, with: fn(other) { #(x, other) })
      [first_combinations, ..do_combination_pairs(xs)]
    }
  }
}

/// Return unique pair combinations of elements in the list
///
/// ## Examples
///
/// ```
/// > combination_pairs([1, 2, 3])
/// [#(1, 2), #(1, 3), #(2, 3)]
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
/// ```
/// > list.interleave([[1, 2], [101, 102], [201, 202]])
/// [1, 101, 201, 2, 102, 202]
/// ```
///
pub fn interleave(list: List(List(a))) -> List(a) {
  transpose(list)
  |> flatten
}

/// Transpose rows and columns of the list of lists.
///
/// ## Examples
///
/// ```
/// > transpose([[1, 2, 3], [101, 102, 103]])
/// [[1, 101], [2, 102], [3, 103]]
/// ```
pub fn transpose(list_of_list: List(List(a))) -> List(List(a)) {
  let take_first = fn(list) {
    case list {
      [] -> []
      [f] -> [f]
      [f, .._rest] -> [f]
    }
  }

  case list_of_list {
    [] -> []
    [[], ..xss] -> transpose(xss)
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
