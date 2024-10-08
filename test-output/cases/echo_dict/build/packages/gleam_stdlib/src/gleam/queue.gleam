import gleam/list

/// A queue is an ordered collection of elements. It is similar to a list, but
/// unlike a list elements can be added to or removed from either the front or
/// the back in a performant fashion.
///
/// The internal representation may be different for two queues with the same
/// elements in the same order if the queues were constructed in different
/// ways. This is the price paid for a queue's fast access at both the front
/// and the back.
///
/// Because of unpredictable internal representation the equality operator `==`
/// may return surprising results, and the `is_equal` and `is_logically_equal`
/// functions are the recommended way to test queues for equality.
///
pub opaque type Queue(a) {
  Queue(in: List(a), out: List(a))
}

/// Creates a fresh queue that contains no values.
///
pub fn new() -> Queue(a) {
  Queue(in: [], out: [])
}

/// Converts a list of elements into a queue of the same elements in the same
/// order. The first element in the list becomes the front element in the queue.
///
/// This function runs in constant time.
///
/// # Examples
///
/// ```gleam
/// [1, 2, 3] |> from_list |> length
/// // -> 3
/// ```
///
pub fn from_list(list: List(a)) -> Queue(a) {
  Queue(in: [], out: list)
}

/// Converts a queue of elements into a list of the same elements in the same
/// order. The front element in the queue becomes the first element in the list.
///
/// This function runs in linear time.
///
/// # Examples
///
/// ```gleam
/// new() |> push_back(1) |> push_back(2) |> to_list
/// // -> [1, 2]
/// ```
///
pub fn to_list(queue: Queue(a)) -> List(a) {
  queue.out
  |> list.append(list.reverse(queue.in))
}

/// Determines whether or not the queue is empty.
///
/// This function runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// [] |> from_list |> is_empty
/// // -> True
/// ```
///
/// ```gleam
/// [1] |> from_list |> is_empty
/// // -> False
/// ```
///
/// ```gleam
/// [1, 2] |> from_list |> is_empty
/// // -> False
/// ```
///
pub fn is_empty(queue: Queue(a)) -> Bool {
  queue.in == [] && queue.out == []
}

/// Counts the number of elements in a given queue.
///
/// This function has to traverse the queue to determine the number of elements,
/// so it runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// length(from_list([]))
/// // -> 0
/// ```
///
/// ```gleam
/// length(from_list([1]))
/// // -> 1
/// ```
///
/// ```gleam
/// length(from_list([1, 2]))
/// // -> 2
/// ```
///
pub fn length(queue: Queue(a)) -> Int {
  list.length(queue.in) + list.length(queue.out)
}

/// Pushes an element onto the back of the queue.
///
/// # Examples
///
/// ```gleam
/// [1, 2] |> from_list |> push_back(3) |> to_list
/// // -> [1, 2, 3]
/// ```
///
pub fn push_back(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: [item, ..queue.in], out: queue.out)
}

/// Pushes an element onto the front of the queue.
///
/// # Examples
///
/// ```gleam
/// [0, 0] |> from_list |> push_front(1) |> to_list
/// // -> [1, 0, 0]
/// ```
///
pub fn push_front(onto queue: Queue(a), this item: a) -> Queue(a) {
  Queue(in: queue.in, out: [item, ..queue.out])
}

/// Gets the last element from the queue, returning the
/// element and a new queue without that element.
///
/// This function typically runs in constant time, but will occasionally run in
/// linear time.
///
/// # Examples
///
/// ```gleam
/// new()
/// |> push_back(0)
/// |> push_back(1)
/// |> pop_back
/// // -> Ok(#(1, push_front(new(), 0)))
/// ```
///
/// ```gleam
/// new()
/// |> push_front(0)
/// |> pop_back
/// // -> Ok(#(0, new()))
/// ```
///
/// ```gleam
/// new() |> pop_back
/// // -> Error(Nil)
/// ```
///
pub fn pop_back(from queue: Queue(a)) -> Result(#(a, Queue(a)), Nil) {
  case queue {
    Queue(in: [], out: []) -> Error(Nil)
    Queue(in: [], out: out) -> pop_back(Queue(in: list.reverse(out), out: []))
    Queue(in: [first, ..rest], out: out) -> {
      let queue = Queue(in: rest, out: out)
      Ok(#(first, queue))
    }
  }
}

/// Gets the first element from the queue, returning the
/// element and a new queue without that element.
///
/// This function typically runs in constant time, but will occasionally run in
/// linear time.
///
/// # Examples
///
/// ```gleam
/// new()
/// |> push_front(1)
/// |> push_front(0)
/// |> pop_front
/// // -> Ok(#(0, push_back(new(), 1)))
/// ```
///
/// ```gleam
/// new()
/// |> push_back(0)
/// |> pop_front
/// // -> Ok(#(0, new()))
/// ```
///
/// ```gleam
/// new() |> pop_back
/// // -> Error(Nil)
/// ```
///
pub fn pop_front(from queue: Queue(a)) -> Result(#(a, Queue(a)), Nil) {
  case queue {
    Queue(in: [], out: []) -> Error(Nil)
    Queue(in: in, out: []) -> pop_front(Queue(in: [], out: list.reverse(in)))
    Queue(in: in, out: [first, ..rest]) -> {
      let queue = Queue(in: in, out: rest)
      Ok(#(first, queue))
    }
  }
}

/// Creates a new queue from a given queue containing the same elements, but in
/// the opposite order.
///
/// This function runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// [] |> from_list |> reverse |> to_list
/// // -> []
/// ```
///
/// ```gleam
/// [1] |> from_list |> reverse |> to_list
/// // -> [1]
/// ```
///
/// ```gleam
/// [1, 2] |> from_list |> reverse |> to_list
/// // -> [2, 1]
/// ```
///
pub fn reverse(queue: Queue(a)) -> Queue(a) {
  Queue(in: queue.out, out: queue.in)
}

fn check_equal(
  xs: List(a),
  x_tail: List(a),
  ys: List(a),
  y_tail: List(a),
  eq: fn(a, a) -> Bool,
) -> Bool {
  case xs, x_tail, ys, y_tail {
    [], [], [], [] -> True
    [x, ..xs], _, [y, ..ys], _ ->
      case eq(x, y) {
        False -> False
        True -> check_equal(xs, x_tail, ys, y_tail, eq)
      }
    [], [_, ..], _, _ -> check_equal(list.reverse(x_tail), [], ys, y_tail, eq)
    _, _, [], [_, ..] -> check_equal(xs, x_tail, list.reverse(y_tail), [], eq)
    _, _, _, _ -> False
  }
}

/// Checks whether two queues have equal elements in the same order, where the
/// equality of elements is determined by a given equality checking function.
///
/// This function is useful as the internal representation may be different for
/// two queues with the same elements in the same order depending on how they
/// were constructed, so the equality operator `==` may return surprising
/// results.
///
/// This function runs in linear time multiplied by the time taken by the
/// element equality checking function.
///
pub fn is_logically_equal(
  a: Queue(a),
  to b: Queue(a),
  checking element_is_equal: fn(a, a) -> Bool,
) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, element_is_equal)
}

/// Checks whether two queues have the same elements in the same order.
///
/// This function is useful as the internal representation may be different for
/// two queues with the same elements in the same order depending on how they
/// were constructed, so the equality operator `==` may return surprising
/// results.
///
/// This function runs in linear time.
///
pub fn is_equal(a: Queue(a), to b: Queue(a)) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, fn(a, b) { a == b })
}
