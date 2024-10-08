import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order

// Internal private representation of an Iterator
type Action(element) {
  // Dedicated to Electric Six
  // https://youtu.be/_30t2dzEgiw?t=162
  Stop
  Continue(element, fn() -> Action(element))
}

/// An iterator is a lazily evaluated sequence of element.
///
/// Iterators are useful when working with collections that are too large to
/// fit in memory (or those that are infinite in size) as they only require the
/// elements currently being processed to be in memory.
///
/// As a lazy data structure no work is done when an iterator is filtered,
/// mapped, etc, instead a new iterator is returned with these transformations
/// applied to the stream. Once the stream has all the required transformations
/// applied it can be evaluated using functions such as `fold` and `to_list`.
///
pub opaque type Iterator(element) {
  Iterator(continuation: fn() -> Action(element))
}

// Public API for iteration
pub type Step(element, accumulator) {
  Next(element: element, accumulator: accumulator)
  Done
}

// Shortcut for an empty iterator.
fn stop() -> Action(element) {
  Stop
}

// Creating Iterators
fn do_unfold(
  initial: acc,
  f: fn(acc) -> Step(element, acc),
) -> fn() -> Action(element) {
  fn() {
    case f(initial) {
      Next(x, acc) -> Continue(x, do_unfold(acc, f))
      Done -> Stop
    }
  }
}

/// Creates an iterator from a given function and accumulator.
///
/// The function is called on the accumulator and returns either `Done`,
/// indicating the iterator has no more elements, or `Next` which contains a
/// new element and accumulator. The element is yielded by the iterator and the
/// new accumulator is used with the function to compute the next element in
/// the sequence.
///
/// ## Examples
///
/// ```gleam
/// unfold(from: 5, with: fn(n) {
///  case n {
///    0 -> Done
///    n -> Next(element: n, accumulator: n - 1)
///  }
/// })
/// |> to_list
/// // -> [5, 4, 3, 2, 1]
/// ```
///
pub fn unfold(
  from initial: acc,
  with f: fn(acc) -> Step(element, acc),
) -> Iterator(element) {
  initial
  |> do_unfold(f)
  |> Iterator
}

/// Creates an iterator that yields values created by calling a given function
/// repeatedly.
///
/// ```gleam
/// repeatedly(fn() { 7 })
/// |> take(3)
/// |> to_list
/// // -> [7, 7, 7]
/// ```
///
pub fn repeatedly(f: fn() -> element) -> Iterator(element) {
  unfold(Nil, fn(_) { Next(f(), Nil) })
}

/// Creates an iterator that returns the same value infinitely.
///
/// ## Examples
///
/// ```gleam
/// repeat(10)
/// |> take(4)
/// |> to_list
/// // -> [10, 10, 10, 10]
/// ```
///
pub fn repeat(x: element) -> Iterator(element) {
  repeatedly(fn() { x })
}

/// Creates an iterator that yields each element from the given list.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn from_list(list: List(element)) -> Iterator(element) {
  let yield = fn(acc) {
    case acc {
      [] -> Done
      [head, ..tail] -> Next(head, tail)
    }
  }
  unfold(list, yield)
}

// Consuming Iterators
fn do_transform(
  continuation: fn() -> Action(a),
  state: acc,
  f: fn(acc, a) -> Step(b, acc),
) -> fn() -> Action(b) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(el, next) ->
        case f(state, el) {
          Done -> Stop
          Next(yield, next_state) ->
            Continue(yield, do_transform(next, next_state, f))
        }
    }
  }
}

/// Creates an iterator from an existing iterator
/// and a stateful function that may short-circuit.
///
/// `f` takes arguments `acc` for current state and `el` for current element from underlying iterator,
/// and returns either `Next` with yielded element and new state value, or `Done` to halt the iterator.
///
/// ## Examples
///
/// Approximate implementation of `index` in terms of `transform`:
///
/// ```gleam
/// from_list(["a", "b", "c"])
/// |> transform(0, fn(i, el) { Next(#(i, el), i + 1) })
/// |> to_list
/// // -> [#(0, "a"), #(1, "b"), #(2, "c")]
/// ```
///
pub fn transform(
  over iterator: Iterator(a),
  from initial: acc,
  with f: fn(acc, a) -> Step(b, acc),
) -> Iterator(b) {
  do_transform(iterator.continuation, initial, f)
  |> Iterator
}

fn do_fold(
  continuation: fn() -> Action(e),
  f: fn(acc, e) -> acc,
  accumulator: acc,
) -> acc {
  case continuation() {
    Continue(elem, next) -> do_fold(next, f, f(accumulator, elem))
    Stop -> accumulator
  }
}

/// Reduces an iterator of elements into a single value by calling a given
/// function on each element in turn.
///
/// If called on an iterator of infinite length then this function will never
/// return.
///
/// If you do not care about the end value and only wish to evaluate the
/// iterator for side effects consider using the `run` function instead.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> fold(from: 0, with: fn(acc, element) { element + acc })
/// // -> 10
/// ```
///
pub fn fold(
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(acc, e) -> acc,
) -> acc {
  iterator.continuation
  |> do_fold(f, initial)
}

// TODO: test
/// Evaluates all elements emitted by the given iterator. This function is useful for when
/// you wish to trigger any side effects that would occur when evaluating
/// the iterator.
///
pub fn run(iterator: Iterator(e)) -> Nil {
  fold(iterator, Nil, fn(_, _) { Nil })
}

/// Evaluates an iterator and returns all the elements as a list.
///
/// If called on an iterator of infinite length then this function will never
/// return.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3])
/// |> map(fn(x) { x * 2 })
/// |> to_list
/// // -> [2, 4, 6]
/// ```
///
pub fn to_list(iterator: Iterator(element)) -> List(element) {
  iterator
  |> fold([], fn(acc, e) { [e, ..acc] })
  |> list.reverse
}

/// Eagerly accesses the first value of an iterator, returning a `Next`
/// that contains the first value and the rest of the iterator.
///
/// If called on an empty iterator, `Done` is returned.
///
/// ## Examples
///
/// ```gleam
/// let assert Next(first, rest) = from_list([1, 2, 3, 4]) |> step
///
/// first
/// // -> 1
///
/// rest |> to_list
/// // -> [2, 3, 4]
/// ```
///
/// ```gleam
/// empty() |> step
/// // -> Done
/// ```
///
pub fn step(iterator: Iterator(e)) -> Step(e, Iterator(e)) {
  case iterator.continuation() {
    Stop -> Done
    Continue(e, a) -> Next(e, Iterator(a))
  }
}

fn do_take(continuation: fn() -> Action(e), desired: Int) -> fn() -> Action(e) {
  fn() {
    case desired > 0 {
      False -> Stop
      True ->
        case continuation() {
          Stop -> Stop
          Continue(e, next) -> Continue(e, do_take(next, desired - 1))
        }
    }
  }
}

/// Creates an iterator that only yields the first `desired` elements.
///
/// If the iterator does not have enough elements all of them are yielded.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> take(up_to: 3)
/// |> to_list
/// // -> [1, 2, 3]
/// ```
///
/// ```gleam
/// from_list([1, 2])
/// |> take(up_to: 3)
/// |> to_list
/// // -> [1, 2]
/// ```
///
pub fn take(from iterator: Iterator(e), up_to desired: Int) -> Iterator(e) {
  iterator.continuation
  |> do_take(desired)
  |> Iterator
}

fn do_drop(continuation: fn() -> Action(e), desired: Int) -> Action(e) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case desired > 0 {
        True -> do_drop(next, desired - 1)
        False -> Continue(e, next)
      }
  }
}

/// Evaluates and discards the first N elements in an iterator, returning a new
/// iterator.
///
/// If the iterator does not have enough elements an empty iterator is
/// returned.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> drop(up_to: 3)
/// |> to_list
/// // -> [4, 5]
/// ```
///
/// ```gleam
/// from_list([1, 2])
/// |> drop(up_to: 3)
/// |> to_list
/// // -> []
/// ```
///
pub fn drop(from iterator: Iterator(e), up_to desired: Int) -> Iterator(e) {
  fn() { do_drop(iterator.continuation, desired) }
  |> Iterator
}

fn do_map(continuation: fn() -> Action(a), f: fn(a) -> b) -> fn() -> Action(b) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, continuation) -> Continue(f(e), do_map(continuation, f))
    }
  }
}

/// Creates an iterator from an existing iterator and a transformation function.
///
/// Each element in the new iterator will be the result of calling the given
/// function on the elements in the given iterator.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3])
/// |> map(fn(x) { x * 2 })
/// |> to_list
/// // -> [2, 4, 6]
/// ```
///
pub fn map(over iterator: Iterator(a), with f: fn(a) -> b) -> Iterator(b) {
  iterator.continuation
  |> do_map(f)
  |> Iterator
}

fn do_map2(
  continuation1: fn() -> Action(a),
  continuation2: fn() -> Action(b),
  with fun: fn(a, b) -> c,
) -> fn() -> Action(c) {
  fn() {
    case continuation1() {
      Stop -> Stop
      Continue(a, next_a) ->
        case continuation2() {
          Stop -> Stop
          Continue(b, next_b) ->
            Continue(fun(a, b), do_map2(next_a, next_b, fun))
        }
    }
  }
}

/// Combines two iterators into a single one using the given function.
///
/// If an iterator is longer than the other the extra elements are dropped.
///
/// This function does not evaluate the elements of the two iterators, the
/// computation is performed when the resulting iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// let first = from_list([1, 2, 3])
/// let second = from_list([4, 5, 6])
/// map2(first, second, fn(x, y) { x + y }) |> to_list
/// // -> [5, 7, 9]
/// ```
///
/// ```gleam
/// let first = from_list([1, 2])
/// let second = from_list(["a", "b", "c"])
/// map2(first, second, fn(i, x) { #(i, x) }) |> to_list
/// // -> [#(1, "a"), #(2, "b")]
/// ```
///
pub fn map2(
  iterator1: Iterator(a),
  iterator2: Iterator(b),
  with fun: fn(a, b) -> c,
) -> Iterator(c) {
  do_map2(iterator1.continuation, iterator2.continuation, fun)
  |> Iterator
}

fn do_append(first: fn() -> Action(a), second: fn() -> Action(a)) -> Action(a) {
  case first() {
    Continue(e, first) -> Continue(e, fn() { do_append(first, second) })
    Stop -> second()
  }
}

/// Appends two iterators, producing a new iterator.
///
/// This function does not evaluate the elements of the iterators, the
/// computation is performed when the resulting iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2])
/// |> append(from_list([3, 4]))
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn append(to first: Iterator(a), suffix second: Iterator(a)) -> Iterator(a) {
  fn() { do_append(first.continuation, second.continuation) }
  |> Iterator
}

fn do_flatten(flattened: fn() -> Action(Iterator(a))) -> Action(a) {
  case flattened() {
    Stop -> Stop
    Continue(it, next_iterator) ->
      do_append(it.continuation, fn() { do_flatten(next_iterator) })
  }
}

/// Flattens an iterator of iterators, creating a new iterator.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([[1, 2], [3, 4]])
/// |> map(from_list)
/// |> flatten
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn flatten(iterator: Iterator(Iterator(a))) -> Iterator(a) {
  fn() { do_flatten(iterator.continuation) }
  |> Iterator
}

/// Joins a list of iterators into a single iterator.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// [[1, 2], [3, 4]]
/// |> map(from_list)
/// |> concat
/// |> to_list
/// // -> [1, 2, 3, 4]
/// ```
///
pub fn concat(iterators: List(Iterator(a))) -> Iterator(a) {
  flatten(from_list(iterators))
}

/// Creates an iterator from an existing iterator and a transformation function.
///
/// Each element in the new iterator will be the result of calling the given
/// function on the elements in the given iterator and then flattening the
/// results.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2])
/// |> flat_map(fn(x) { from_list([x, x + 1]) })
/// |> to_list
/// // -> [1, 2, 2, 3]
/// ```
///
pub fn flat_map(
  over iterator: Iterator(a),
  with f: fn(a) -> Iterator(b),
) -> Iterator(b) {
  iterator
  |> map(f)
  |> flatten
}

fn do_filter(
  continuation: fn() -> Action(e),
  predicate: fn(e) -> Bool,
) -> Action(e) {
  case continuation() {
    Stop -> Stop
    Continue(e, iterator) ->
      case predicate(e) {
        True -> Continue(e, fn() { do_filter(iterator, predicate) })
        False -> do_filter(iterator, predicate)
      }
  }
}

/// Creates an iterator from an existing iterator and a predicate function.
///
/// The new iterator will contain elements from the first iterator for which
/// the given function returns `True`.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// import gleam/int
///
/// from_list([1, 2, 3, 4])
/// |> filter(int.is_even)
/// |> to_list
/// // -> [2, 4]
/// ```
///
pub fn filter(
  iterator: Iterator(a),
  keeping predicate: fn(a) -> Bool,
) -> Iterator(a) {
  fn() { do_filter(iterator.continuation, predicate) }
  |> Iterator
}

fn do_filter_map(
  continuation: fn() -> Action(a),
  f: fn(a) -> Result(b, c),
) -> Action(b) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case f(e) {
        Ok(e) -> Continue(e, fn() { do_filter_map(next, f) })
        Error(_) -> do_filter_map(next, f)
      }
  }
}

/// Creates an iterator from an existing iterator and a transforming predicate function.
///
/// The new iterator will contain elements from the first iterator for which
/// the given function returns `Ok`, transformed to the value inside the `Ok`.
///
/// This function does not evaluate the elements of the iterator, the
/// computation is performed when the iterator is later run.
///
/// ## Examples
///
/// ```gleam
/// import gleam/string
/// import gleam/int
///
/// "a1b2c3d4e5f"
/// |> string.to_graphemes
/// |> from_list
/// |> filter_map(int.parse)
/// |> to_list
/// // -> [1, 2, 3, 4, 5]
/// ```
///
pub fn filter_map(
  iterator: Iterator(a),
  keeping_with f: fn(a) -> Result(b, c),
) -> Iterator(b) {
  fn() { do_filter_map(iterator.continuation, f) }
  |> Iterator
}

/// Creates an iterator that repeats a given iterator infinitely.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2])
/// |> cycle
/// |> take(6)
/// |> to_list
/// // -> [1, 2, 1, 2, 1, 2]
/// ```
///
pub fn cycle(iterator: Iterator(a)) -> Iterator(a) {
  repeat(iterator)
  |> flatten
}

/// Creates an iterator of ints, starting at a given start int and stepping by
/// one to a given end int.
///
/// ## Examples
///
/// ```gleam
/// range(from: 1, to: 5) |> to_list
/// // -> [1, 2, 3, 4, 5]
/// ```
///
/// ```gleam
/// range(from: 1, to: -2) |> to_list
/// // -> [1, 0, -1, -2]
/// ```
///
/// ```gleam
/// range(from: 0, to: 0) |> to_list
/// // -> [0]
/// ```
///
pub fn range(from start: Int, to stop: Int) -> Iterator(Int) {
  case int.compare(start, stop) {
    order.Eq -> once(fn() { start })
    order.Gt ->
      unfold(from: start, with: fn(current) {
        case current < stop {
          False -> Next(current, current - 1)
          True -> Done
        }
      })

    order.Lt ->
      unfold(from: start, with: fn(current) {
        case current > stop {
          False -> Next(current, current + 1)
          True -> Done
        }
      })
  }
}

fn do_find(continuation: fn() -> Action(a), f: fn(a) -> Bool) -> Result(a, Nil) {
  case continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      case f(e) {
        True -> Ok(e)
        False -> do_find(next, f)
      }
  }
}

/// Finds the first element in a given iterator for which the given function returns
/// `True`.
///
/// Returns `Error(Nil)` if the function does not return `True` for any of the
/// elements.
///
/// ## Examples
///
/// ```gleam
/// find(from_list([1, 2, 3]), fn(x) { x > 2 })
/// // -> Ok(3)
/// ```
///
/// ```gleam
/// find(from_list([1, 2, 3]), fn(x) { x > 4 })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find(empty(), fn(_) { True })
/// // -> Error(Nil)
/// ```
///
pub fn find(
  in haystack: Iterator(a),
  one_that is_desired: fn(a) -> Bool,
) -> Result(a, Nil) {
  haystack.continuation
  |> do_find(is_desired)
}

fn do_find_map(
  continuation: fn() -> Action(a),
  f: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  case continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      case f(e) {
        Ok(e) -> Ok(e)
        Error(_) -> do_find_map(next, f)
      }
  }
}

/// Finds the first element in a given iterator
/// for which the given function returns `Ok(new_value)`,
/// then returns the wrapped `new_value`.
///
/// Returns `Error(Nil)` if no such element is found.
///
/// ## Examples
///
/// ```gleam
/// find_map(from_list(["a", "1", "2"]), int.parse)
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// find_map(from_list(["a", "b", "c"]), int.parse)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// find_map(from_list([]), int.parse)
/// // -> Error(Nil)
/// ```
///
pub fn find_map(
  in haystack: Iterator(a),
  one_that is_desired: fn(a) -> Result(b, c),
) -> Result(b, Nil) {
  haystack.continuation
  |> do_find_map(is_desired)
}

fn do_index(
  continuation: fn() -> Action(element),
  next: Int,
) -> fn() -> Action(#(element, Int)) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, continuation) ->
        Continue(#(e, next), do_index(continuation, next + 1))
    }
  }
}

/// Wraps values yielded from an iterator with indices, starting from 0.
///
/// ## Examples
///
/// ```gleam
/// from_list(["a", "b", "c"]) |> index |> to_list
/// // -> [#("a", 0), #("b", 1), #("c", 2)]
/// ```
///
pub fn index(over iterator: Iterator(element)) -> Iterator(#(element, Int)) {
  iterator.continuation
  |> do_index(0)
  |> Iterator
}

/// Creates an iterator that infinitely applies a function to a value.
///
/// ## Examples
///
/// ```gleam
/// iterate(1, fn(n) { n * 3 }) |> take(5) |> to_list
/// // -> [1, 3, 9, 27, 81]
/// ```
///
pub fn iterate(
  from initial: element,
  with f: fn(element) -> element,
) -> Iterator(element) {
  unfold(initial, fn(element) { Next(element, f(element)) })
}

fn do_take_while(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> fn() -> Action(element) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(e, next) ->
        case predicate(e) {
          False -> Stop
          True -> Continue(e, do_take_while(next, predicate))
        }
    }
  }
}

/// Creates an iterator that yields elements while the predicate returns `True`.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 2, 4])
/// |> take_while(satisfying: fn(x) { x < 3 })
/// |> to_list
/// // -> [1, 2]
/// ```
///
pub fn take_while(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Iterator(element) {
  iterator.continuation
  |> do_take_while(predicate)
  |> Iterator
}

fn do_drop_while(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Action(element) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) ->
      case predicate(e) {
        False -> Continue(e, next)
        True -> do_drop_while(next, predicate)
      }
  }
}

/// Creates an iterator that drops elements while the predicate returns `True`,
/// and then yields the remaining elements.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 2, 5])
/// |> drop_while(satisfying: fn(x) { x < 4 })
/// |> to_list
/// // -> [4, 2, 5]
/// ```
///
pub fn drop_while(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Iterator(element) {
  fn() { do_drop_while(iterator.continuation, predicate) }
  |> Iterator
}

fn do_scan(
  continuation: fn() -> Action(element),
  f: fn(acc, element) -> acc,
  accumulator: acc,
) -> fn() -> Action(acc) {
  fn() {
    case continuation() {
      Stop -> Stop
      Continue(el, next) -> {
        let accumulated = f(accumulator, el)
        Continue(accumulated, do_scan(next, f, accumulated))
      }
    }
  }
}

/// Creates an iterator from an existing iterator and a stateful function.
///
/// Specifically, this behaves like `fold`, but yields intermediate results.
///
/// ## Examples
///
/// ```gleam
/// // Generate a sequence of partial sums
/// from_list([1, 2, 3, 4, 5])
/// |> scan(from: 0, with: fn(acc, el) { acc + el })
/// |> to_list
/// // -> [1, 3, 6, 10, 15]
/// ```
///
pub fn scan(
  over iterator: Iterator(element),
  from initial: acc,
  with f: fn(acc, element) -> acc,
) -> Iterator(acc) {
  iterator.continuation
  |> do_scan(f, initial)
  |> Iterator
}

fn do_zip(
  left: fn() -> Action(a),
  right: fn() -> Action(b),
) -> fn() -> Action(#(a, b)) {
  fn() {
    case left() {
      Stop -> Stop
      Continue(el_left, next_left) ->
        case right() {
          Stop -> Stop
          Continue(el_right, next_right) ->
            Continue(#(el_left, el_right), do_zip(next_left, next_right))
        }
    }
  }
}

/// Zips two iterators together, emitting values from both
/// until the shorter one runs out.
///
/// ## Examples
///
/// ```gleam
/// from_list(["a", "b", "c"])
/// |> zip(range(20, 30))
/// |> to_list
/// // -> [#("a", 20), #("b", 21), #("c", 22)]
/// ```
///
pub fn zip(left: Iterator(a), right: Iterator(b)) -> Iterator(#(a, b)) {
  do_zip(left.continuation, right.continuation)
  |> Iterator
}

// Result of collecting a single chunk by key
type Chunk(element, key) {
  AnotherBy(List(element), key, element, fn() -> Action(element))
  LastBy(List(element))
}

fn next_chunk(
  continuation: fn() -> Action(element),
  f: fn(element) -> key,
  previous_key: key,
  current_chunk: List(element),
) -> Chunk(element, key) {
  case continuation() {
    Stop -> LastBy(list.reverse(current_chunk))
    Continue(e, next) -> {
      let key = f(e)
      case key == previous_key {
        True -> next_chunk(next, f, key, [e, ..current_chunk])
        False -> AnotherBy(list.reverse(current_chunk), key, e, next)
      }
    }
  }
}

fn do_chunk(
  continuation: fn() -> Action(element),
  f: fn(element) -> key,
  previous_key: key,
  previous_element: element,
) -> Action(List(element)) {
  case next_chunk(continuation, f, previous_key, [previous_element]) {
    LastBy(chunk) -> Continue(chunk, stop)
    AnotherBy(chunk, key, el, next) ->
      Continue(chunk, fn() { do_chunk(next, f, key, el) })
  }
}

/// Creates an iterator that emits chunks of elements
/// for which `f` returns the same value.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 2, 3, 4, 4, 6, 7, 7])
/// |> chunk(by: fn(n) { n % 2 })
/// |> to_list
/// // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
/// ```
///
pub fn chunk(
  over iterator: Iterator(element),
  by f: fn(element) -> key,
) -> Iterator(List(element)) {
  fn() {
    case iterator.continuation() {
      Stop -> Stop
      Continue(e, next) -> do_chunk(next, f, f(e), e)
    }
  }
  |> Iterator
}

// Result of collecting a single sized chunk
type SizedChunk(element) {
  Another(List(element), fn() -> Action(element))
  Last(List(element))
  NoMore
}

fn next_sized_chunk(
  continuation: fn() -> Action(element),
  left: Int,
  current_chunk: List(element),
) -> SizedChunk(element) {
  case continuation() {
    Stop ->
      case current_chunk {
        [] -> NoMore
        remaining -> Last(list.reverse(remaining))
      }
    Continue(e, next) -> {
      let chunk = [e, ..current_chunk]
      case left > 1 {
        False -> Another(list.reverse(chunk), next)
        True -> next_sized_chunk(next, left - 1, chunk)
      }
    }
  }
}

fn do_sized_chunk(
  continuation: fn() -> Action(element),
  count: Int,
) -> fn() -> Action(List(element)) {
  fn() {
    case next_sized_chunk(continuation, count, []) {
      NoMore -> Stop
      Last(chunk) -> Continue(chunk, stop)
      Another(chunk, next_element) ->
        Continue(chunk, do_sized_chunk(next_element, count))
    }
  }
}

/// Creates an iterator that emits chunks of given size.
///
/// If the last chunk does not have `count` elements, it is yielded
/// as a partial chunk, with less than `count` elements.
///
/// For any `count` less than 1 this function behaves as if it was set to 1.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5, 6])
/// |> sized_chunk(into: 2)
/// |> to_list
/// // -> [[1, 2], [3, 4], [5, 6]]
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5, 6, 7, 8])
/// |> sized_chunk(into: 3)
/// |> to_list
/// // -> [[1, 2, 3], [4, 5, 6], [7, 8]]
/// ```
///
pub fn sized_chunk(
  over iterator: Iterator(element),
  into count: Int,
) -> Iterator(List(element)) {
  iterator.continuation
  |> do_sized_chunk(count)
  |> Iterator
}

fn do_intersperse(
  continuation: fn() -> Action(element),
  separator: element,
) -> Action(element) {
  case continuation() {
    Stop -> Stop
    Continue(e, next) -> {
      let next_interspersed = fn() { do_intersperse(next, separator) }
      Continue(separator, fn() { Continue(e, next_interspersed) })
    }
  }
}

/// Creates an iterator that yields the given `elem` element
/// between elements emitted by the underlying iterator.
///
/// ## Examples
///
/// ```gleam
/// empty()
/// |> intersperse(with: 0)
/// |> to_list
/// // -> []
/// ```
///
/// ```gleam
/// from_list([1])
/// |> intersperse(with: 0)
/// |> to_list
/// // -> [1]
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> intersperse(with: 0)
/// |> to_list
/// // -> [1, 0, 2, 0, 3, 0, 4, 0, 5]
/// ```
///
pub fn intersperse(
  over iterator: Iterator(element),
  with elem: element,
) -> Iterator(element) {
  fn() {
    case iterator.continuation() {
      Stop -> Stop
      Continue(e, next) -> Continue(e, fn() { do_intersperse(next, elem) })
    }
  }
  |> Iterator
}

fn do_any(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Bool {
  case continuation() {
    Stop -> False
    Continue(e, next) ->
      case predicate(e) {
        True -> True
        False -> do_any(next, predicate)
      }
  }
}

/// Returns `True` if any element emitted by the iterator satisfies the given predicate,
/// `False` otherwise.
///
/// This function short-circuits once it finds a satisfying element.
///
/// An empty iterator results in `False`.
///
/// ## Examples
///
/// ```gleam
/// empty()
/// |> any(fn(n) { n % 2 == 0 })
/// // -> False
/// ```
///
/// ```gleam
/// from_list([1, 2, 5, 7, 9])
/// |> any(fn(n) { n % 2 == 0 })
/// // -> True
/// ```
///
/// ```gleam
/// from_list([1, 3, 5, 7, 9])
/// |> any(fn(n) { n % 2 == 0 })
/// // -> False
/// ```
///
pub fn any(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Bool {
  iterator.continuation
  |> do_any(predicate)
}

fn do_all(
  continuation: fn() -> Action(element),
  predicate: fn(element) -> Bool,
) -> Bool {
  case continuation() {
    Stop -> True
    Continue(e, next) ->
      case predicate(e) {
        True -> do_all(next, predicate)
        False -> False
      }
  }
}

/// Returns `True` if all elements emitted by the iterator satisfy the given predicate,
/// `False` otherwise.
///
/// This function short-circuits once it finds a non-satisfying element.
///
/// An empty iterator results in `True`.
///
/// ## Examples
///
/// ```gleam
/// empty()
/// |> all(fn(n) { n % 2 == 0 })
/// // -> True
/// ```
///
/// ```gleam
/// from_list([2, 4, 6, 8])
/// |> all(fn(n) { n % 2 == 0 })
/// // -> True
/// ```
///
/// ```gleam
/// from_list([2, 4, 5, 8])
/// |> all(fn(n) { n % 2 == 0 })
/// // -> False
/// ```
///
pub fn all(
  in iterator: Iterator(element),
  satisfying predicate: fn(element) -> Bool,
) -> Bool {
  iterator.continuation
  |> do_all(predicate)
}

fn update_group_with(el: element) -> fn(Option(List(element))) -> List(element) {
  fn(maybe_group) {
    case maybe_group {
      Some(group) -> [el, ..group]
      None -> [el]
    }
  }
}

fn group_updater(
  f: fn(element) -> key,
) -> fn(Dict(key, List(element)), element) -> Dict(key, List(element)) {
  fn(groups, elem) {
    groups
    |> dict.upsert(f(elem), update_group_with(elem))
  }
}

/// Returns a `Dict(k, List(element))` of elements from the given iterator
/// grouped with the given key function.
///
/// The order within each group is preserved from the iterator.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5, 6])
/// |> group(by: fn(n) { n % 3 })
/// // -> dict.from_list([#(0, [3, 6]), #(1, [1, 4]), #(2, [2, 5])])
/// ```
///
pub fn group(
  in iterator: Iterator(element),
  by key: fn(element) -> key,
) -> Dict(key, List(element)) {
  iterator
  |> fold(dict.new(), group_updater(key))
  |> dict.map_values(fn(_, group) { list.reverse(group) })
}

/// This function acts similar to fold, but does not take an initial state.
/// Instead, it starts from the first yielded element
/// and combines it with each subsequent element in turn using the given function.
/// The function is called as `f(accumulator, current_element)`.
///
/// Returns `Ok` to indicate a successful run, and `Error` if called on an empty iterator.
///
/// ## Examples
///
/// ```gleam
/// from_list([])
/// |> reduce(fn(acc, x) { acc + x })
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4, 5])
/// |> reduce(fn(acc, x) { acc + x })
/// // -> Ok(15)
/// ```
///
pub fn reduce(
  over iterator: Iterator(e),
  with f: fn(e, e) -> e,
) -> Result(e, Nil) {
  case iterator.continuation() {
    Stop -> Error(Nil)
    Continue(e, next) ->
      do_fold(next, f, e)
      |> Ok
  }
}

/// Returns the last element in the given iterator.
///
/// Returns `Error(Nil)` if the iterator is empty.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// empty() |> last
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// range(1, 10) |> last
/// // -> Ok(10)
/// ```
///
pub fn last(iterator: Iterator(element)) -> Result(element, Nil) {
  iterator
  |> reduce(fn(_, elem) { elem })
}

/// Creates an iterator that yields no elements.
///
/// ## Examples
///
/// ```gleam
/// empty() |> to_list
/// // -> []
/// ```
///
pub fn empty() -> Iterator(element) {
  Iterator(stop)
}

/// Creates an iterator that yields exactly one element provided by calling the given function.
///
/// ## Examples
///
/// ```gleam
/// once(fn() { 1 }) |> to_list
/// // -> [1]
/// ```
///
pub fn once(f: fn() -> element) -> Iterator(element) {
  fn() { Continue(f(), stop) }
  |> Iterator
}

/// Creates an iterator that yields the given element exactly once.
///
/// ## Examples
///
/// ```gleam
/// single(1) |> to_list
/// // -> [1]
/// ```
///
pub fn single(elem: element) -> Iterator(element) {
  once(fn() { elem })
}

fn do_interleave(
  current: fn() -> Action(element),
  next: fn() -> Action(element),
) -> Action(element) {
  case current() {
    Stop -> next()
    Continue(e, next_other) ->
      Continue(e, fn() { do_interleave(next, next_other) })
  }
}

/// Creates an iterator that alternates between the two given iterators
/// until both have run out.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> interleave(from_list([11, 12, 13, 14]))
/// |> to_list
/// // -> [1, 11, 2, 12, 3, 13, 4, 14]
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4])
/// |> interleave(from_list([100]))
/// |> to_list
/// // -> [1, 100, 2, 3, 4]
/// ```
///
pub fn interleave(
  left: Iterator(element),
  with right: Iterator(element),
) -> Iterator(element) {
  fn() { do_interleave(left.continuation, right.continuation) }
  |> Iterator
}

fn do_fold_until(
  continuation: fn() -> Action(e),
  f: fn(acc, e) -> list.ContinueOrStop(acc),
  accumulator: acc,
) -> acc {
  case continuation() {
    Stop -> accumulator
    Continue(elem, next) ->
      case f(accumulator, elem) {
        list.Continue(accumulator) -> do_fold_until(next, f, accumulator)
        list.Stop(accumulator) -> accumulator
      }
  }
}

/// Like `fold`, `fold_until` reduces an iterator of elements into a single value by calling a given
/// function on each element in turn, but uses `list.ContinueOrStop` to determine
/// whether or not to keep iterating.
///
/// If called on an iterator of infinite length then this function will only ever
/// return if the function returns `list.Stop`.
///
/// ## Examples
///
/// ```gleam
/// import gleam/list
///
/// let f = fn(acc, e) {
///   case e {
///     _ if e < 4 -> list.Continue(e + acc)
///     _ -> list.Stop(acc)
///   }
/// }
///
/// from_list([1, 2, 3, 4])
/// |> fold_until(from: 0, with: f)
/// // -> 6
/// ```
///
pub fn fold_until(
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(acc, e) -> list.ContinueOrStop(acc),
) -> acc {
  iterator.continuation
  |> do_fold_until(f, initial)
}

fn do_try_fold(
  over continuation: fn() -> Action(a),
  with f: fn(acc, a) -> Result(acc, err),
  from accumulator: acc,
) -> Result(acc, err) {
  case continuation() {
    Stop -> Ok(accumulator)
    Continue(elem, next) -> {
      case f(accumulator, elem) {
        Ok(result) -> do_try_fold(next, f, result)
        Error(_) as error -> error
      }
    }
  }
}

/// A variant of fold that might fail.
///
/// The folding function should return `Result(accumulator, error)`.
/// If the returned value is `Ok(accumulator)` try_fold will try the next value in the iterator.
/// If the returned value is `Error(error)` try_fold will stop and return that error.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4])
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
  over iterator: Iterator(e),
  from initial: acc,
  with f: fn(acc, e) -> Result(acc, err),
) -> Result(acc, err) {
  iterator.continuation
  |> do_try_fold(f, initial)
}

/// Returns the first element yielded by the given iterator, if it exists,
/// or `Error(Nil)` otherwise.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3]) |> first
/// // -> Ok(1)
/// ```
///
/// ```gleam
/// empty() |> first
/// // -> Error(Nil)
/// ```
pub fn first(from iterator: Iterator(e)) -> Result(e, Nil) {
  case iterator.continuation() {
    Stop -> Error(Nil)
    Continue(e, _) -> Ok(e)
  }
}

/// Returns nth element yielded by the given iterator, where `0` means the first element.
///
/// If there are not enough elements in the iterator, `Error(Nil)` is returned.
///
/// For any `index` less than `0` this function behaves as if it was set to `0`.
///
/// ## Examples
///
/// ```gleam
/// from_list([1, 2, 3, 4]) |> at(2)
/// // -> Ok(3)
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4]) |> at(4)
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// empty() |> at(0)
/// // -> Error(Nil)
/// ```
///
pub fn at(in iterator: Iterator(e), get index: Int) -> Result(e, Nil) {
  iterator
  |> drop(index)
  |> first
}

fn do_length(over continuation: fn() -> Action(e), with length: Int) -> Int {
  case continuation() {
    Stop -> length
    Continue(_, next) -> do_length(next, length + 1)
  }
}

/// Counts the number of elements in the given iterator.
///
/// This function has to traverse the entire iterator to count its elements,
/// so it runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// empty() |> length
/// // -> 0
/// ```
///
/// ```gleam
/// from_list([1, 2, 3, 4]) |> length
/// // -> 4
/// ```
///
pub fn length(over iterator: Iterator(e)) -> Int {
  iterator.continuation
  |> do_length(0)
}

/// Traverse an iterator, calling a function on each element.
///
/// ## Examples
///
/// ```gleam
/// empty() |> each(io.println)
/// // -> Nil
/// ```
///
/// ```gleam
/// from_list(["Tom", "Malory", "Louis"]) |> each(io.println)
/// // -> Nil
/// // Tom
/// // Malory
/// // Louis
/// ```
///
pub fn each(over iterator: Iterator(a), with f: fn(a) -> b) -> Nil {
  iterator
  |> map(f)
  |> run
}

/// Add a new element to the start of an iterator.
///
/// This function is for use with `use` expressions, to replicate the behaviour
/// of the `yield` keyword found in other languages.
///
/// ## Examples
///
/// ```gleam
/// let iterator = {
///   use <- yield(1)
///   use <- yield(2)
///   use <- yield(3)
///   empty()
/// }
///
/// iterator |> to_list
/// // -> [1, 2, 3]
/// ```
///
pub fn yield(element: a, next: fn() -> Iterator(a)) -> Iterator(a) {
  Iterator(fn() { Continue(element, fn() { next().continuation() }) })
}
