// Based on "Purely Functional Data Structures" by Okasaki (1998)

import gleam/order.{Gt, Order}

type T(a) {
  E
  T(Int, a, T(a), T(a))
}

pub opaque type Heap(a) {
  Heap(root: T(a), compare: fn(a, a) -> Order)
}

pub fn new(compare: fn(a, a) -> Order) -> Heap(a) {
  Heap(E, compare)
}

pub fn insert(heap: Heap(a), item: a) -> Heap(a) {
  Heap(merge(T(1, item, E, E), heap.root, heap.compare), heap.compare)
}

pub fn find_min(heap: Heap(a)) -> Result(a, Nil) {
  case heap.root {
    T(_, x, _, _) -> Ok(x)
    E -> Error(Nil)
  }
}

pub fn delete_min(heap: Heap(a)) -> Result(#(a, Heap(a)), Nil) {
  case heap.root {
    T(_, x, a, b) -> Ok(#(x, Heap(merge(a, b, heap.compare), heap.compare)))
    E -> Error(Nil)
  }
}

fn merge(h1: T(a), h2: T(a), compare: fn(a, a) -> Order) -> T(a) {
  case h1, h2 {
    h, E -> h
    E, h -> h
    T(_, x, a1, b1), T(_, y, a2, b2) ->
      case compare(x, y) {
        Gt -> make(y, a2, merge(h1, b2, compare))
        _ -> make(x, a1, merge(b1, h2, compare))
      }
  }
}

fn make(x, a, b) {
  let rank_a = case a {
    T(r, _, _, _) -> r
    E -> 0
  }
  let rank_b = case b {
    T(r, _, _, _) -> r
    E -> 0
  }
  case rank_a < rank_b {
    True -> T(rank_a + 1, x, b, a)
    _ -> T(rank_b + 1, x, a, b)
  }
}
