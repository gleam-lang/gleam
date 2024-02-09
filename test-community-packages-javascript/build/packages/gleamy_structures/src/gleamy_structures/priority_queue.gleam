import gleam/order.{Order}
import gleam/list
import gleamy_structures/heap/pairing_heap as heap

type Queue(a) =
  heap.Heap(a)

pub fn from_list(list: List(a), compare: fn(a, a) -> Order) -> Queue(a) {
  list.fold(list, new(compare), heap.insert)
}

pub fn is_empty(queue: Queue(a)) -> Bool {
  case heap.find_min(queue) {
    Ok(_) -> False
    Error(_) -> True
  }
}

pub fn count(queue: Queue(a)) -> Int {
  case heap.delete_min(queue) {
    Ok(#(_, q)) -> count(q) + 1
    Error(_) -> 0
  }
}

pub fn new(compare: fn(a, a) -> Order) -> Queue(a) {
  heap.new(compare)
}

pub fn pop(from queue: Queue(a)) -> Result(#(a, Queue(a)), Nil) {
  heap.delete_min(queue)
}

pub fn peek(from queue: Queue(a)) -> Result(a, Nil) {
  heap.find_min(queue)
}

pub fn push(onto queue: Queue(a), this item: a) -> Queue(a) {
  heap.insert(queue, item)
}

pub fn reorder(queue: Queue(a), compare: fn(a, a) -> Order) -> Queue(a) {
  case heap.delete_min(queue) {
    Ok(#(x, q)) -> heap.insert(reorder(q, compare), x)
    Error(_) -> heap.new(compare)
  }
}

pub fn to_list(queue: Queue(a)) -> List(a) {
  case heap.delete_min(queue) {
    Ok(#(x, q)) -> [x, ..to_list(q)]
    Error(_) -> []
  }
}
